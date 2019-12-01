{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import           Brick
import           Brick.BChan (newBChan, BChan, writeBChan)
import qualified Brick.Focus as Focus
import           Brick.Widgets.Border (hBorder)
import qualified Brick.Widgets.List as L
import           Control.Concurrent.Async (async, forConcurrently_)
import           Control.Exception (SomeException)
import           Control.Exception.Base (try)
import           Control.Lens
import           Control.Monad (void, mfilter, when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.CaseInsensitive as CI
import           Data.Default (def)
import           Data.Foldable (for_)
import           Data.List (isPrefixOf, partition, find)
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, POSIXTime)
import           Data.Time.LocalTime
       (utcToLocalTime, getCurrentTimeZone, TimeZone, utcToLocalTime)
import           Dhall (input,auto)
import           Formatting (sformat, (%), (%.))
import qualified Formatting as F
import qualified Formatting.Time as F
import           Graphics.Vty (Event, mkVty, Key(KChar), Event(EvKey))
import qualified Graphics.Vty as Vty
import           Graphics.Vty.Input.Events (Key(KEnter))
import           Network.HTTP.Client
       (HttpException(HttpExceptionRequest),
        HttpExceptionContent(StatusCodeException), responseHeaders)
import           Network.URI
import           System.Environment (getArgs)
import           System.Exit (exitSuccess, exitFailure)
import           System.Process
       (shell, createProcess, CreateProcess, waitForProcess)
import           System.Process.Internals (StdStream(CreatePipe))
import           Text.Printf (printf)

import           Events
import           Network.Pocket
import           Network.Pocket.Meta (fetchRedditCommentCount)
import           Network.Pocket.Retrieve
import           Network.Pocket.Ui.State
import           Network.Pocket.Ui.Widgets

makeLensesFor [("std_err", "stdErr"), ("std_out", "stdOut")] ''CreateProcess

trigger :: BChan HocketEvent -> HocketEvent -> IO ()
trigger = writeBChan

vtyEventHandler :: BChan HocketEvent
                -> HocketState
                -> Event
                -> EventM Name (Next HocketState)
vtyEventHandler es s (EvKey (KChar ' ') []) = do
  liftIO . for_ (focusedItem s) $ \pit -> es `trigger` browseItemEvt pit
  continue s
vtyEventHandler es s (EvKey KEnter []) = do
  liftIO . for_ (focusedItem s) $ \pit -> do
    es `trigger` browseItemEvt pit
    es `trigger` shiftItemEvt (pit ^. itemId)
  continue s
vtyEventHandler es s (EvKey (KChar 'u') []) = do
  liftIO $ es `trigger` fetchItemsEvt
  continue s
vtyEventHandler es s (EvKey (KChar 'A') []) = do
  liftIO $ es `trigger` archiveItemsEvt
  continue s
vtyEventHandler es s (EvKey (KChar 'd') []) = do
  liftIO . for_ (focusedItem s) $ \pit ->
    es `trigger` shiftItemEvt (pit ^. itemId)
  continue s
vtyEventHandler _ s (EvKey (KChar 'q') []) = halt s
vtyEventHandler es s (EvKey (KChar 'r') []) = do
  liftIO $ for_ (focusedItem s) $ \pit -> es `trigger` getRedditCommentsEvt [pit]
  continue s
vtyEventHandler es s (EvKey (KChar 'R') []) = do
  let items = s ^.. hsContents . each . _2
      redditItems = filter (isRedditUrl . view resolvedUrl) items
  liftIO $ when (not (null redditItems)) $ es `trigger` getRedditCommentsEvt redditItems
  continue s
vtyEventHandler _ s (EvKey (KChar '\t') []) =
  s & focusRing %~ Focus.focusNext & continue
vtyEventHandler _ s e =
  continue =<<
  case focused s of
    Just ItemListName -> handleEventLensed s itemListVi handleViListEvent e
    Just PendingListName ->
      handleEventLensed s pendingListVi handleViListEvent e
    _ -> return s

internalEventHandler
  :: BChan HocketEvent
  -> HocketState
  -> HocketEvent
  -> EventM Name (Next HocketState)
internalEventHandler es s (HocketAsync e) = asyncCommandEventHandler es s e
internalEventHandler es s (HocketUi e) = uiCommandEventHandler es s e

asyncCommandEventHandler
  :: BChan HocketEvent
  -> HocketState
  -> AsyncCommand
  -> EventM Name (Next HocketState)
asyncCommandEventHandler es s@(view hsAsync -> Nothing) FetchItems = do
  fetchAsync <-
    liftIO . async $ do
      es `trigger` setStatusEvt (Just "fetching")
      eitherErrorPis <- retrieveItems (s ^. hsCredentials) (s ^. hsLastUpdated)
      case eitherErrorPis of
        Left e ->
          es `trigger` asyncActionFailedEvt (errorMessageFromException e)
        Right (PocketItemBatch ts pis) -> do
          es `trigger` setStatusEvt Nothing
          es `trigger` fetchedItemsEvt ts pis
  continue (s & hsAsync ?~ fetchAsync)
asyncCommandEventHandler _ s FetchItems = continue s
asyncCommandEventHandler _ s (FetchedItems ts pis) = do
  let (newItems, toBeDeleted) = partition ((== Normal) . view status) pis
      newS = (s & insertItems newItems & removeItems (toBeDeleted ^.. each . itemId) &
                  hsAsync .~
                  Nothing &
                  hsLastUpdated ?~
                  ts)
  continue newS

asyncCommandEventHandler es s (AsyncActionFailed err) = do
  liftIO (es `trigger` setStatusEvt (Just ("failed" <> maybe "" (": " <>) err)))
  continue (s & hsAsync .~ Nothing)
asyncCommandEventHandler es s@(view hsAsync -> Nothing) ArchiveItems =
  case hsNumItems s of
    (_, 0) -> continue s
    (_, _) -> do
      archiveAsync <-
        liftIO . async $ do
          es `trigger` setStatusEvt (Just "archiving")
          eitherErrorResults <-
            performArchive (s ^. hsCredentials) (s ^.. pendingList . L.listElementsL . each)
          case eitherErrorResults of
            Left e ->
              es `trigger` asyncActionFailedEvt (errorMessageFromException e)
            Right results -> do
              es `trigger`
                archivedItemsEvt
                  (mapMaybe
                     (\(pit, successful) ->
                        mfilter (const successful) (Just (view itemId pit)))
                     results)
              es `trigger` setStatusEvt Nothing
      continue (s & hsAsync ?~ archiveAsync)
asyncCommandEventHandler _ s ArchiveItems = continue s
asyncCommandEventHandler _ s (ArchivedItems pis) =
  continue (s & removeItems pis & hsAsync .~ Nothing)
asyncCommandEventHandler es s@(view hsAsync -> Nothing) (GetRedditCommentCount items) = do
  liftIO $ es `trigger` setStatusEvt (Just "getting comment counts")
  let maybeSubredditsAndIds = mapMaybe (\i -> (view itemId i,) <$> subredditAndArticleId i) items
  asyncGet <- liftIO . async $ do
    forConcurrently_ maybeSubredditsAndIds $ \(pid, (subreddit, articleId)) -> do
      eitherErrCount <-(tryHttpException $ fetchRedditCommentCount subreddit articleId)
      for_ eitherErrCount $ \count -> es `trigger` gotRedditCommentsEvt pid count
      es `trigger` doneWithRedditCommentsEvt
    es `trigger` setStatusEvt Nothing
  continue (s & hsAsync ?~ asyncGet)
asyncCommandEventHandler _ s (GetRedditCommentCount _) = continue s
asyncCommandEventHandler _ s (GotRedditCommentCount pid (RedditCommentCount count)) = do
  continue (s & hsContents . at pid . _Just . _2 . redditCommentCount ?~ count)
asyncCommandEventHandler _ s DoneWithRedditComments =
  continue (s & hsAsync .~ Nothing)

uiCommandEventHandler :: BChan HocketEvent
                      -> HocketState
                      -> UiCommand
                      -> EventM Name (Next HocketState)
uiCommandEventHandler _ s (ShiftItem pid) = continue (toggleStatus pid s)
uiCommandEventHandler _ s (RemoveItems pis) = continue (removeItems pis s)
uiCommandEventHandler _ s (SetStatus t) = continue (s & hsStatus .~ t)
uiCommandEventHandler es s (BrowseItem pit) = do
  res <- liftIO . try @SomeException $ browseItem "firefox '%s'" (pit ^. resolvedUrl)
  case res of
    Left e -> liftIO $ es `trigger` setStatusEvt (Just (T.pack $ show e))
    Right () -> return ()
  continue s

eventHandler
  :: BChan HocketEvent
  -> HocketState
  -> BrickEvent Name HocketEvent
  -> EventM Name (Next HocketState)
eventHandler es s (VtyEvent e) = vtyEventHandler es s e
eventHandler es s (AppEvent e) = internalEventHandler es s e
eventHandler _ s _ = continue s

main :: IO ()
main = do
  args <- getArgs
  cred <- input auto "./config.dhall"
  case length args of
    1 -> addToPocket cred (head args)
    0 -> do
      events <- newBChan 10
      tz <- getCurrentTimeZone
      vty <- mkVty Vty.defaultConfig
      void
        (customMain
           vty
           (mkVty Vty.defaultConfig)
           (Just events)
           (app tz events)
           (initialState cred))
    _ -> do
      putStrLn $ "Invalid args: " ++ show args
      exitFailure

addToPocket :: PocketCredentials -> String -> IO ()
addToPocket cred url = do
  putStrLn url
  res <-
    tryHttpException . runHocket (cred, def) . pocket . AddItem $
    T.pack url
  case res of
    Left e -> do
      putStrLn $ "Error during request: " ++ show e
      exitFailure
    Right False -> do
      putStrLn "Could not add item."
      exitFailure
    Right True -> exitSuccess

app :: TimeZone -> BChan HocketEvent -> App HocketState HocketEvent Name
app tz events =
  App
  { appDraw = drawGui tz
  , appChooseCursor = Focus.focusRingCursor (view focusRing)
  , appHandleEvent = \s e -> fmap syncForRender <$> eventHandler events s e
  , appStartEvent =
      \s -> do
        liftIO (events `trigger` fetchItemsEvt)
        return s
  , appAttrMap = const hocketAttrMap
  }

hocketAttrMap :: AttrMap
hocketAttrMap =
  attrMap
    Vty.defAttr
    [ ("list" <> "selected" <> "focused", boldBlackOnOrange)
    , ("list" <> "unselectedItem", whiteFg)
    , ( "bar"
      , Vty.defAttr `Vty.withBackColor` Vty.black `Vty.withForeColor` Vty.white)
    ]

drawGui :: TimeZone -> HocketState -> [Widget Name]
drawGui tz s = [w]
  where
    w =
      vBox
        [ hBar
            ("Hocket: (" <>
             uncurry (sformat (F.int % "|" % F.int)) (hsNumItems s) <>
             ")")
        , L.renderList
            listDrawElement
            (isFocused s ItemListName)
            (s ^. itemList)
        , hBorder
        , vLimit 10 $
          L.renderList
            listDrawElement
            (isFocused s PendingListName)
            (s ^. pendingList)
        , hBar " " <+>
          withAttr
            "bar"
            (padLeft
               Max
               (txt
                  (maybe
                     "<never>"
                     (sformat F.hms . utcToLocalTime tz . posixSecondsToUTCTime)
                     (s ^. hsLastUpdated))))
        , txt (fromMaybe " " (s ^. hsStatus))
        ]

focused :: HocketState -> Maybe Name
focused = Focus.focusGetCurrent . view focusRing

focusedList :: HocketState -> Maybe (L.List Name PocketItem)
focusedList s =
  case focused s of
    Just n
      | n == ItemListName -> s ^? itemList
    Just n
      | n == PendingListName -> s ^? pendingList
    _ -> Nothing

isFocused :: HocketState -> Name -> Bool
isFocused s name = maybe False (== name) (focused s)

listDrawElement :: Bool -> PocketItem -> Widget Name
listDrawElement sel e =
  (if sel
     then withAttr ("list" <> "selectedItem")
     else withAttr ("list" <> "unselectedItem"))
    (padRight Max (txtDisplay e))

orange :: Vty.Color
orange = Vty.rgbColor 215 135 (0 :: Int)

boldBlackOnOrange :: Vty.Attr
boldBlackOnOrange =
  Vty.defAttr `Vty.withForeColor` black `Vty.withBackColor` orange `Vty.withStyle`
  Vty.bold

black :: Vty.Color
black = Vty.rgbColor zero zero zero
  where
    zero = 0 :: Int

whiteFg :: Vty.Attr
whiteFg = Vty.defAttr `Vty.withForeColor` Vty.white

hBar :: Text -> Widget Name
hBar = withAttr "bar" . padRight Max . txt

retrieveItems :: PocketCredentials -> Maybe POSIXTime -> IO (Either HttpException PocketItemBatch)
retrieveItems cred =
  tryHttpException .
    runHocket (cred, def) .
    pocket .
    RetrieveItems .
    maybe retrieveAllUnread retrieveDeltaSince
  where
    retrieveAllUnread = defaultRetrieval
    retrieveDeltaSince ts =
      defaultRetrieval & retrieveSince ?~ ts & retrieveState .~ All

performArchive :: PocketCredentials -> [PocketItem] -> IO (Either HttpException [(PocketItem, Bool)])
performArchive cred items =
  tryHttpException . fmap (zip items) . runHocket (cred, def) .
  pocket $
  Batch (map (Archive . view itemId) items)

tryHttpException :: IO a -> IO (Either HttpException a)
tryHttpException = try @HttpException

defaultRetrieval :: RetrieveConfig
defaultRetrieval =
  def & retrieveSort ?~ NewestFirst & retrieveCount .~ NoLimit &
  retrieveDetailType ?~
  Complete

txtDisplay :: PocketItem -> Widget Name
txtDisplay pit =
  txt (T.justifyRight 10 ' ' leftEdge) <+>
  txt
    (fromMaybe
       "<empty>"
       (listToMaybe $ filter (not . T.null) [given, resolved, T.pack url])) <+>
  padLeft Max (hLimit horizontalUriLimit (txt trimmedUrl)) <+>
  txt commentCount
  where
    resolved = view resolvedTitle pit
    given = view givenTitle pit
    (URL url) = view resolvedUrl pit
    added = posixSecondsToUTCTime (view timeAdded pit)
    leftEdge =
      sformat (F.year % "-" <> F.month % "-" <> F.dayOfMonth) added <> ": "
    commentCount =
      fromMaybe "     " $
      fmap
        (sformat ("  " % (F.left 3 ' ' %. F.int) % ""))
        (pit ^. redditCommentCount)
    trimmedUrl = T.pack (trimURI url)

horizontalUriLimit :: Int
horizontalUriLimit = 60

trimURI :: String -> String
trimURI uri =
  fromMaybe uri $ do
    parsed <- parseURI uri
    auth <- uriAuthority parsed
    return
      (strip
         "reddit.com/"
         (strip "www." (uriRegName auth) <> uriPath parsed <> uriQuery parsed))
  where
    strip prefix s =
      if prefix `isPrefixOf` s
        then drop (length prefix) s
        else s

focusedItem :: HocketState -> Maybe PocketItem
focusedItem s = do
  list <- focusedList s
  snd <$> L.listSelectedElement list

browseItem :: String -> URL -> IO ()
browseItem shellCmd (URL url) = do
  let spec = shell $ printf shellCmd url
  (_, _, _, ph) <- createProcess $ spec & stdOut .~ CreatePipe & stdErr .~ CreatePipe
  void . waitForProcess $ ph

errorMessageFromException :: HttpException -> Maybe Text
errorMessageFromException (HttpExceptionRequest _ (StatusCodeException resp _)) =
  T.decodeUtf8 . snd <$>
  find (\(k, _) -> k == CI.mk "x-error") (responseHeaders resp)
errorMessageFromException e = Just . T.pack $ show e
