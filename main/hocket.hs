{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

import           Control.Applicative ((<*>), pure)
import           Control.Concurrent (forkIO, MVar, takeMVar, readMVar, putMVar, newMVar)
import           Control.Concurrent.Async (async, Async, poll, cancel)
import           Control.Exception (try)
import           Control.Lens (view)
import           Control.Monad (join, void, replicateM_)
import           Control.Monad.Error (runErrorT)
import           Control.Monad.IO.Class (liftIO)
import           Data.ConfigFile
import           Data.Default
import           Data.Foldable (traverse_, for_, for_)
import           Data.Functor ((<$>))
import           Data.List (sortBy, sortBy, (\\))
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (Traversable, for)
import           Graphics.Vty
import           Graphics.Vty.Widgets.All
import           Network.HTTP.Client (HttpException)
import           Numeric.Natural
import           System.Environment (getArgs)
import           System.Exit (exitSuccess)
import           System.Process
import           Text.Printf (printf)

import           Pocket
import           Printing
import           Types

main :: IO ()
main = do
  Just (creds,cmd) <- readFromConfig "hocket.cfg"
  args <- (fmap . fmap ) T.pack getArgs
  let (dispatch,rest) = (head args, tail args)
  runHocket (creds,def) $ case dispatch of
    "get" -> liftIO . newestFirst =<< performGet (read . T.unpack . head $ rest)
    "add" -> traverse_ (perform . AddItem) rest
    "gui" -> liftIO . vty cmd creds $ []
    _ -> fail "Invalid args."

performGet :: Maybe (Natural,Natural) -> HocketCA [PocketItem]
performGet maybeOffsetCount = do
  retrieved <- perform $ RetrieveItems maybeOffsetCount
  return . sortBy (flip . comparing $ view timeAdded) $ retrieved

readFromConfig :: FilePath -> IO (Maybe (PocketCredentials, String))
readFromConfig path = do

  eitherErrorTriple <- runErrorT $ do
    cp <- join $ liftIO $ readfile emptyCP path
    consumerKey <- get cp "Credentials" "consumer_key"
    accessToken <- get cp "Credentials" "access_token"
    cmd <- get cp "Launch" "launch_cmd"
    return ( AccessToken $ accessToken
           , ConsumerKey $ consumerKey
           , cmd
           )
  return $ case eitherErrorTriple of
    Right (token,key,shCmd) -> Just (PocketCredentials key token, shCmd)
    Left _ -> Nothing

browseItem :: String -> T.Text -> IO ()
browseItem shellCmd url = do
  let spec = shell $ printf shellCmd (T.unpack url)
      spec' = spec {std_out = CreatePipe, std_err = CreatePipe}
  void $ createProcess spec'

addLstItem :: Widget (List PocketItem FormattedText) -> PocketItem -> IO ()
addLstItem lst itm = addToList lst itm =<< (plainText . bestTitle $ itm)

bestTitle :: PocketItem -> Text
bestTitle itm =
  view (if view givenTitle itm /= "" then givenTitle else resolvedTitle) itm

data HocketGUI = HocketGUI { unreadLst :: Widget (List PocketItem FormattedText)
                           , toArchiveLst :: Widget (List PocketItem FormattedText)
                           , helpBar :: Widget FormattedText
                           , statusBar :: Widget FormattedText
                           , guiCreds :: PocketCredentials
                           , launchCommand :: String
                           , mainFocusGroup :: Widget FocusGroup
                           , titleText :: Widget FormattedText
                           , asyncAction :: MVar (Async ())
                           }

abortAsync :: HocketGUI -> IO ()
abortAsync gui@(asyncAction -> m) = do
  v <- readMVar m
  stat <- poll v
  case stat of
    Nothing -> cancel v >> updateStatusBar gui "Aborted"
    Just _ -> return ()

tryAsync :: HocketGUI -> IO () -> IO ()
tryAsync (asyncAction -> m) act = do
  finished <- poll =<< readMVar m
  case finished of
    Nothing -> return ()
    Just _ -> takeMVar m >> (putMVar m =<< async act)

insertPocketItems :: Traversable f =>
                     Widget (List PocketItem FormattedText)
                     -> f PocketItem -> IO ()
insertPocketItems lst = traverse_ (addLstItem lst)

sortList :: Widget (List PocketItem FormattedText) -> IO ()
sortList lst = do
  sel <- getSelected lst
  pis <- sortBy (flip . comparing $ view timeAdded) <$> extractAndClear lst
  insertPocketItems lst pis
  for_ sel $ \(pos, _) -> setSelected lst pos

getAllItems :: Widget (List a b) -> IO [a]
getAllItems lst = do
  n <- getListSize lst
  for [0..(n-1)] $ \i -> do
    Just (itm, _) <- getListItem lst i
    return itm

extractAndClear :: Widget (List a b) -> IO [a]
extractAndClear lst = do
  itms <- getAllItems lst
  clearList lst
  return itms

updateStatusBar :: HocketGUI -> T.Text -> IO ()
updateStatusBar gui txt = schedule $ setText (statusBar gui) txt

retrieveNewItems :: HocketGUI -> IO ()
retrieveNewItems gui = do
  tryAsync gui $ do
    updateStatusBar gui "Updating"
    oldPIs <- (++) <$> (getAllItems $ unreadLst gui) <*> (getAllItems $ toArchiveLst gui)
    eitherErrorPIs <-
      tryHttpException $ runHocket (guiCreds gui, def) $ performGet Nothing
    case eitherErrorPIs of
      Right pis -> schedule $ do
        insertPocketItems (unreadLst gui) $ pis \\ oldPIs
        sortList (unreadLst gui)
        updateStatusBar gui ""
      Left _ -> updateStatusBar gui "Updating failed"

removeItemFromLst :: Eq a => Widget (List a b) -> a -> IO ()
removeItemFromLst lst itm = do
  maybePos <- listFindFirst lst itm
  traverse_ (removeFromList lst) maybePos

executeArchiveAction :: HocketGUI -> IO ()
executeArchiveAction gui = do
  tryAsync gui $ do
    updateStatusBar gui "Archiving"
    let archiveLst = toArchiveLst gui
    itms <- getAllItems archiveLst
    res <- performArchive itms archiveLst
    updateStatusBar gui
             . either (const "Archieving failed") (const "")
             $ res
  where
    performArchive itms archiveLst =
      tryHttpException $ runHocket (guiCreds gui, def) $ do
        bs <- perform $ Batch (map (Archive . view itemId) itms)
        let archivedItms = map fst $ filter snd $ zip itms bs
        liftIO . schedule $ for_ archivedItms $ \itm -> do
          removeItemFromLst archiveLst itm


keepCurrent :: Attr
keepCurrent = Attr KeepCurrent KeepCurrent KeepCurrent

boldBlackOnOrange :: Attr
boldBlackOnOrange = realBlack `on` (Color240 147) `mergeAttr` style bold
  where realBlack = rgb_color (0::Int) 0 0

createGUI :: String -> PocketCredentials -> IO (HocketGUI, Collection)
createGUI shCmd cred = do
   gui <- HocketGUI <$> (newList keepCurrent 1)
                    <*> (newList keepCurrent 1)
                    <*> (plainText . T.intercalate " | " $ [ "q:Quit"
                                                           , "d:Shift item"
                                                           , "D:Shift all"
                                                           , "u:Update"
                                                           , "A:Archive pending"
                                                           , "C:Cancel"
                                                           , "SPC: Launch"
                                                           , "Enter:Launch & Shift"
                                                           ])
                   <*> plainText ""
                   <*> pure cred
                   <*> pure shCmd
                   <*> newFocusGroup
                   <*> plainText "Hocket"
                   <*> (newMVar =<< (async $ return ()))

   bottomBar <- ((pure $ helpBar gui) <++> hFill ' ' 1 <++> (pure $ statusBar gui))
   topBar <- ((pure $ titleText gui) <++> hFill ' ' 1)

   setNormalAttribute (bottomBar) $ Attr KeepCurrent KeepCurrent (SetTo black)
   setNormalAttribute (topBar) $ Attr KeepCurrent KeepCurrent (SetTo black)
   setNormalAttribute (statusBar gui) $ Attr (SetTo bold) KeepCurrent KeepCurrent

   setFocusAttribute (unreadLst gui) boldBlackOnOrange
   setFocusAttribute (toArchiveLst gui) boldBlackOnOrange
   for_ [unreadLst,toArchiveLst] $ \selector ->
     setNormalAttribute (selector gui) $ Attr KeepCurrent (SetTo white) KeepCurrent
   for_ [helpBar, statusBar] $ \selector ->
     setNormalAttribute (selector gui) $ Attr KeepCurrent (SetTo white) KeepCurrent

   ui <- centered =<< pure topBar
                 <--> (pure $ unreadLst gui)
                 <--> hBorder
                 <--> (vFixed 10 (toArchiveLst gui))
                 <--> pure bottomBar

   let fg = mainFocusGroup gui
   void $ addToFocusGroup fg (unreadLst gui)
   void $ addToFocusGroup fg (toArchiveLst gui)

   fg `onKeyPressed` \_ k _ -> case k of
     (KASCII 'q') -> exitSuccess
     (KASCII 'u') -> retrieveNewItems gui >> return True
     (KASCII 'A') -> executeArchiveAction gui >> return True
     _ -> return False
   c <- newCollection
   void $ addToCollection c ui fg
   return (gui,c)

vty :: String -> PocketCredentials -> [PocketItem] -> IO ()
vty cmd cred  pis = do
  (gui,c) <- createGUI cmd cred
  insertPocketItems (unreadLst gui) pis

  for_ [unreadLst gui, toArchiveLst gui] $ \x -> do
    x `onItemActivated` (lstItemActivatedHandler gui x)
    x `onKeyPressed` lstKeyPressedHandler gui

  (unreadLst gui) `onKeyPressed` \this key _ -> case key of
    (KASCII 'd') -> shiftSelected this (toArchiveLst gui) >> return True
    (KASCII 'D') -> do
      insertPocketItems (toArchiveLst gui) =<< extractAndClear this
      focusNext (mainFocusGroup gui)
      sortList (toArchiveLst gui)
      return True
    _ -> return False

  (toArchiveLst gui) `onKeyPressed` \this key _ -> case key of
    (KASCII 'd') -> shiftSelected this (unreadLst gui) >> return True
    (KASCII 'D') -> do
      insertPocketItems (unreadLst gui) =<< extractAndClear this
      focusNext (mainFocusGroup gui)
      sortList (toArchiveLst gui)
      return True

    _ -> return False

  retrieveNewItems gui
  runUi c defaultContext

lstKeyPressedHandler :: HocketGUI
                     -> Widget (List PocketItem FormattedText)
                     -> Key
                     -> t
                     -> IO Bool
lstKeyPressedHandler gui this key _ = case key of
  (KASCII 'j') -> scrollDown this >> return True
  (KASCII 'k') -> scrollUp this >> return True
  (KASCII 'J') -> replicateM_ 3 (scrollDown this) >> return True
  (KASCII 'K') -> replicateM_ 3 (scrollUp this) >> return True
  (KASCII 's') -> sortList this >> return True
  (KASCII 'g') -> scrollToBeginning this >> return True
  (KASCII 'G') -> scrollToEnd this >> return True
  (KASCII 'C') -> abortAsync gui >> return True
  (KASCII ' ') -> do
    void . forkIO $ do
      maybeSel <- getSelected this
      traverse_ (browseItem (launchCommand gui) . view givenUrl . fst . snd) maybeSel
    return True
  _ -> return False

lstItemActivatedHandler :: HocketGUI
                        -> Widget (List PocketItem FormattedText)
                        -> ActivateItemEvent PocketItem t
                        -> IO ()
lstItemActivatedHandler gui src (ActivateItemEvent _ v _) = do
  shiftSelected src (toArchiveLst gui)
  browseItem (launchCommand gui) . view givenUrl $ v

shiftSelected :: Widget (List PocketItem FormattedText)
         -> Widget (List PocketItem FormattedText)
         -> IO ()
shiftSelected this target = do
  sel <- getSelected this
  for_ sel $ \(pos, (val, _)) -> do
    void $ removeFromList this pos
    addLstItem target val
  sortList target

tryHttpException :: IO a -> IO (Either HttpException a)
tryHttpException = try
