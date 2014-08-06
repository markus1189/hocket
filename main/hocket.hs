{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Applicative ((<*>), pure)
import           Control.Concurrent (forkIO, MVar, takeMVar, readMVar, putMVar, newMVar, swapMVar)
import           Control.Concurrent.Async (async, Async, poll, cancel)
import           Control.Exception (try)
import           Control.Lens (view, _Right, preview, preview, act)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad (join, void)
import           Control.Monad.Error (runErrorT)
import           Control.Monad.IO.Class (liftIO)
import           Data.ConfigFile
import           Data.Default
import           Data.Foldable (traverse_, for_)
import qualified Data.Function as F
import           Data.Functor ((<$>))
import           Data.List (sortBy, deleteFirstsBy)
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (Traversable)
import           Graphics.Vty
import           Graphics.Vty.Widgets.All
import           Network.HTTP.Client (HttpException)
import           Numeric.Natural
import           System.Environment (getArgs)
import           System.Exit (exitSuccess)
import           System.Process
import           Text.Printf (printf)
import qualified Text.Trans.Tokenize as TT

import           GUI
import           Pocket
import           Printing
import           Types

makeLensesFor [("std_err", "stdErr"), ("std_out", "stdOut")] ''CreateProcess

newtype ShellCommand = Cmd String


data HocketGUI = HocketGUI { _unreadLst :: Widget (List PocketItem FormattedText)
                           , _toArchiveLst :: Widget (List PocketItem FormattedText)
                           , _helpBar :: Widget FormattedText
                           , _statusBar :: Widget FormattedText
                           , _guiCreds :: PocketCredentials
                           , _launchCommand :: ShellCommand
                           , _mainFocusGroup :: Widget FocusGroup
                           , _titleText :: Widget FormattedText
                           , _asyncAction :: MVar (Async ())
                           }
makeLenses ''HocketGUI

main :: IO ()
main = do
  Just (creds,cmd) <- readFromConfig "hocket.cfg"
  args <- (fmap . fmap ) T.pack getArgs
  let (dispatch,rest) = (head args, tail args)
  runHocket (creds,def) $ case dispatch of
    "get" -> liftIO . newestFirst =<< sortedRetrieve (read . T.unpack . head $ rest)
    "add" -> traverse_ (pocket . AddItem) rest
    "gui" -> liftIO . vty cmd creds $ []
    _ -> fail "Invalid args."

sortedRetrieve :: Maybe (Natural,Natural) -> Hocket [PocketItem]
sortedRetrieve maybeOffsetCount = do
  pocket (RetrieveItems maybeOffsetCount) >>=
    return . sortBy (flip . comparing $ view timeAdded)

readFromConfig :: FilePath -> IO (Maybe (PocketCredentials, ShellCommand))
readFromConfig path = do
  eitherErrorTuple <- runErrorT $ do
    cp <- join $ liftIO $ readfile emptyCP path
    consumerKey <- get cp "Credentials" "consumer_key"
    accessToken <- get cp "Credentials" "access_token"
    cmd <- get cp "Launch" "launch_cmd"
    return ( PocketCredentials (ConsumerKey consumerKey)
                               (AccessToken accessToken)
           , Cmd cmd
           )
  return . preview _Right $ eitherErrorTuple

browseItem :: ShellCommand -> URL -> IO ()
browseItem (Cmd shellCmd) (URL url) = do
  let spec = (shell $ printf shellCmd url)
  void $ createProcess $ spec & stdOut .~ CreatePipe
                              & stdErr .~ CreatePipe

-- used to right align urls in gui using vty-ui formatter 'alignRightAfter' (defined here)
magicMarker :: Text
magicMarker = "<!>"

shortenUrl :: URL -> Text
shortenUrl (URL t) = let parts = T.splitOn "/" . T.pack $ t
                         mainPart = T.intercalate "/" . take 3 $ parts
                         rest = T.intercalate "/" . drop 3 $ parts
                         maxW = 50
                         remaining = (maxW - T.length mainPart) `max` 0
                         postFix = T.take remaining rest
                     in T.intercalate "/" [mainPart, postFix]

displayText :: PocketItem -> Text
displayText i = bestTitle i `T.append`
                " " `T.append`
                magicMarker `T.append`
                shortenUrl (view resolvedUrl i)

alignRightAfter :: Text -> Formatter
alignRightAfter marker = Formatter $ \(DisplayRegion w _) ts -> do
  let currentText = TT.serialize ts
      parts = T.splitOn marker currentText
      neededSpaces = 0 `max` (w - fromIntegral (T.length currentText - T.length marker))
      sp = T.replicate (fromIntegral neededSpaces) " "
      newText = flip TT.tokenize def_attr $ T.intercalate sp parts
  return newText

sortedAddLstItem :: Widget (List PocketItem FormattedText) -> PocketItem -> IO ()
sortedAddLstItem = addToListSortedBy lt (textWidget (alignRightAfter magicMarker) . displayText)
  where
    lt :: PocketItem -> PocketItem -> Ordering
    lt = (flip compare) `F.on` view timeAdded

bestTitle :: PocketItem -> Text
bestTitle itm =
  view (if view givenTitle itm /= "" then givenTitle else resolvedTitle) itm

abortAsync :: HocketGUI -> IO ()
abortAsync gui@(view asyncAction -> m) = do
  v <- readMVar m
  stat <- poll v
  case stat of
    Nothing -> cancel v >> updateStatusBar gui "Aborted"
    Just _ -> return ()

tryAsync :: HocketGUI -> IO () -> IO ()
tryAsync (view asyncAction -> m) a = do
  finished <- poll =<< readMVar m
  case finished of
    Nothing -> return ()
    Just _ -> takeMVar m >> (putMVar m =<< async a)

insertPocketItems :: Traversable f =>
                     Widget (List PocketItem FormattedText)
                     -> f PocketItem -> IO ()
insertPocketItems lst = traverse_ (sortedAddLstItem lst)

extractAndClear :: Widget (List a b) -> IO [a]
extractAndClear lst = do
  itms <- listItems lst
  clearList lst
  return itms

updateStatusBar :: HocketGUI -> T.Text -> IO ()
updateStatusBar gui txt = schedule $ setText (view statusBar gui) txt

retrieveNewItems :: HocketGUI -> IO ()
retrieveNewItems gui = do
  tryAsync gui $ do
    updateStatusBar gui "Updating"
    oldPIs <- (++) <$> (listItems $ view unreadLst gui)
                   <*> (listItems $ view toArchiveLst gui)
    eitherErrorPIs <- tryHttpException
                    . runHocket (view guiCreds gui, def) $ sortedRetrieve Nothing
    case eitherErrorPIs of
      Right pis -> do
        schedule . traverse_ (sortedAddLstItem (view unreadLst gui)) $ pis \\\ oldPIs
        updateStatusBar gui ""
      Left _ -> updateStatusBar gui "Updating failed"
  where (\\\) = deleteFirstsBy idEq

removeItemFromLst :: Eq a => Widget (List a b) -> a -> IO ()
removeItemFromLst lst itm = do
  maybePos <- listFindFirst lst itm
  traverse_ (removeFromList lst) maybePos

executeRenameSelected :: HocketGUI -> Widget (List PocketItem b) -> Text -> IO ()
executeRenameSelected gui w newTxt = withSelection w $ \_ sel -> do
  tryAsync gui $ do
    updateStatusBar gui "Renaming"
    res <- tryHttpException $ runHocket (view guiCreds gui, def) $
      pocket $ RenameItem (view itemId sel) newTxt
    case res of
      Left _ -> sigFail
      Right b -> if b then removeItemFromLst w sel >> sigSucc else sigFail
  where sigFail = updateStatusBar gui "Renaming failed"
        sigSucc = updateStatusBar gui ""

executeArchiveAction :: HocketGUI -> IO ()
executeArchiveAction gui = do
  tryAsync gui $ do
    updateStatusBar gui "Archiving"
    let archiveLst = view toArchiveLst gui
    itms <- listItems archiveLst
    res <- performArchive gui archiveLst itms
    updateStatusBar gui . maybe "" (const "Archieving failed") $ res

performArchive :: HocketGUI
               -> Widget (List PocketItem b)
               -> [PocketItem]
               -> IO (Maybe HttpException)
performArchive gui archiveLst itms = do
  res <- tryHttpException $ runHocket (view guiCreds gui, def) $ do
    bs <- pocket $ Batch (map (Archive . view itemId) itms)
    return . map fst . filter snd $ zip itms bs
  case res of
    Left e -> return $ Just e
    Right archivedItms -> do
      schedule . traverse_ (removeItemFromLst archiveLst) $ archivedItms
      return Nothing

createGUI :: ShellCommand -> PocketCredentials -> IO (HocketGUI, Collection)
createGUI shCmd cred = do
  let normal = Attr KeepCurrent (SetTo white) KeepCurrent
      focusedAttr = boldBlackOnOrange
  gui <- HocketGUI <$> newList' focusedAttr normal
                   <*> newList' focusedAttr normal
                   <*> (plainText . T.intercalate " | " $ [ "q:Quit"
                                                          , "d:Shift item"
                                                          , "D:Shift all"
                                                          , "u:Update"
                                                          , "A:Archive pending"
                                                          , "C:Cancel"
                                                          , "SPC:Launch"
                                                          , "Enter:Launch & Shift"
                                                          ])
                   <*> plainText ""
                   <*> pure cred
                   <*> pure shCmd
                   <*> newFocusGroup
                   <*> plainText "Hocket"
                   <*> (newMVar =<< (async $ return ()))

  bottomBar <- ((pure $ view helpBar gui) <++> hFill ' ' 1 <++> (pure $ view statusBar gui))
  topBar <- ((pure $ view titleText gui) <++> hFill ' ' 1)

  setNormalAttribute (bottomBar) $ Attr KeepCurrent KeepCurrent (SetTo black)
  setNormalAttribute (topBar) $ Attr KeepCurrent KeepCurrent (SetTo black)
  setNormalAttribute (view statusBar gui) $ Attr (SetTo bold) KeepCurrent KeepCurrent

  for_ [helpBar, statusBar] $ \selector ->
    setNormalAttribute (view selector gui) $ Attr KeepCurrent (SetTo white) KeepCurrent

  ui <- centered =<< pure topBar
                <--> (pure $ view unreadLst gui)
                <--> hBorder
                <--> (vFixed 10 (view toArchiveLst gui))
                <--> pure bottomBar

  let fg = view mainFocusGroup gui
  void $ addToFocusGroup fg (view unreadLst gui)
  void $ addToFocusGroup fg (view toArchiveLst gui)

  c <- newCollection

  edlg <- newEditDialog
  displayMainGui <- addToCollection c ui fg
  displayEditDialog <- addToCollection c
    (edlg ^. editDlgDialog & dialogWidget)
    (edlg ^. editDlgFocusGroup)

  setUpEditHandler edlg (view unreadLst gui) displayEditDialog

  (view editDlgDialog edlg) `onDialogCancel` \_ -> displayMainGui
  (view editDlgDialog edlg) `onDialogAccept` \_ -> do
    edlgSrc <- edlg ^! editVar . act readMVar
    if (edlgSrc == Nothing)
      then updateStatusBar gui "Renaming failed"
      else do
        newName <- getEditText (view editDlgWidget edlg)
        Just src <- edlg ^! editVar . act readMVar
        executeRenameSelected gui src newName
    displayMainGui

  fg `onKeyPressed` \_ k _ -> case k of
    (KASCII 'q') -> exitSuccess
    (KASCII 'u') -> retrieveNewItems gui >> return True
    (KASCII 'A') -> executeArchiveAction gui >> return True
    _ -> return False

  return (gui,c)

vty :: ShellCommand -> PocketCredentials -> [PocketItem] -> IO ()
vty cmd cred  pis = do
  (gui,c) <- createGUI cmd cred
  insertPocketItems (view unreadLst gui) pis

  for_ [view unreadLst gui, view toArchiveLst gui] $ \x -> do
    x `onItemActivated` (lstItemActivatedHandler gui x)
    x `onKeyPressed` lstKeyPressedHandler gui

  (view unreadLst gui) `onKeyPressed` \this key _ -> case key of
    (KASCII 'd') -> shiftSelected this (view toArchiveLst gui) >> return True
    (KASCII 'D') -> do
      insertPocketItems (view toArchiveLst gui) =<< extractAndClear this
      focusNext (view mainFocusGroup gui)
      return True
    _ -> return False

  (view toArchiveLst gui) `onKeyPressed` \this key _ -> case key of
    (KASCII 'd') -> shiftSelected this (view unreadLst gui) >> return True
    (KASCII 'D') -> do
      traverse_ (sortedAddLstItem (view unreadLst gui)) =<< extractAndClear this
      focusNext (view mainFocusGroup gui)
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
  (KASCII 'C') -> abortAsync gui >> return True
  (KASCII ' ') -> do
    void . forkIO $ do
      maybeSel <- getSelected this
      traverse_ (browseItem (view launchCommand gui) . view givenUrl . fst . snd) maybeSel
    return True
  _ -> return False

lstItemActivatedHandler :: HocketGUI
                        -> Widget (List PocketItem FormattedText)
                        -> ActivateItemEvent PocketItem t
                        -> IO ()
lstItemActivatedHandler gui src (ActivateItemEvent _ v _) = do
  shiftSelected src (view toArchiveLst gui)
  browseItem (view launchCommand gui) . view givenUrl $ v

shiftSelected :: Widget (List PocketItem FormattedText)
              -> Widget (List PocketItem FormattedText)
              -> IO ()
shiftSelected this target = withSelection this $ \pos val -> do
  void $ removeFromList this pos
  sortedAddLstItem target val

withSelection :: Widget (List a b) -> (Int -> a -> IO c) -> IO ()
withSelection w k = do
  getSelected w >>= traverse_ (\(i, (x, _)) -> k i x)
  return ()

tryHttpException :: IO a -> IO (Either HttpException a)
tryHttpException = try

setUpEditHandler :: EditDialog FormattedText
                 -> Widget (List PocketItem FormattedText)
                 -> IO a
                 -> IO ()
setUpEditHandler e l switch = l `onKeyPressed` \_ k _  -> case k of
  (KASCII 'e') -> do
    withSelection l $ \_ sel -> do
      let edit = e ^. editDlgWidget
      setEditText edit (bestTitle sel)
      void $ e ^! editVar . act (\x -> swapMVar x (Just l))
      switch
    return True
  _ -> return False
