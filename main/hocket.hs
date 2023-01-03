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
import           Control.Lens ( At(at), Each(each), view, _Just, makeLensesFor, Field2(_2) )
import           Control.Lens.Operators
import           Control.Monad (void, mfilter, unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.CaseInsensitive as CI
import           Data.Default (def)
import           Data.Foldable (for_)
import           Data.List (isPrefixOf, partition, find)
import           Data.Maybe (fromMaybe, mapMaybe, isJust)
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

import           Brick.Widgets.List (handleListEventVi, handleListEvent)
import           Control.Lens.Combinators (use)
import           Control.Monad.State (MonadState)
import           Events
import           Network.Pocket
import           Network.Pocket.Meta (fetchRedditCommentCount)
import           Network.Pocket.Retrieve
import           Network.Pocket.Ui.State

makeLensesFor [("std_err", "stdErr"), ("std_out", "stdOut")] ''CreateProcess

trigger :: BChan HocketEvent -> HocketEvent -> IO ()
trigger = writeBChan

vtyEventHandler :: BChan HocketEvent
                -> Event
                -> EventM Name HocketState ()
vtyEventHandler es (EvKey (KChar ' ') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \pit -> es `trigger` browseItemEvt pit
vtyEventHandler es (EvKey KEnter []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \pit -> do
    es `trigger` browseItemEvt pit
    es `trigger` shiftItemEvt (pit ^. itemId)
vtyEventHandler es (EvKey (KChar 'u') []) = do
  liftIO $ es `trigger` fetchItemsEvt
  pure ()
vtyEventHandler es (EvKey (KChar 'A') []) = do
  liftIO $ es `trigger` archiveItemsEvt
  pure ()
vtyEventHandler es (EvKey (KChar 'd') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \pit ->
    es `trigger` shiftItemEvt (pit ^. itemId)
vtyEventHandler _ (EvKey (KChar 'q') []) = halt
vtyEventHandler es (EvKey (KChar 'r') []) = do
  s <- use id
  liftIO $ for_ (focusedItem s) $ \pit -> es `trigger` getRedditCommentsEvt [pit]
vtyEventHandler es (EvKey (KChar 'R') []) = do
  s <- use id
  let items = s ^.. hsContents . each . _2
      redditItems = filter (isRedditUrl . resolvedOrGivenUrl) items
  liftIO $ unless (null redditItems) $ es `trigger` getRedditCommentsEvt redditItems
vtyEventHandler _ (EvKey (KChar '\t') []) = focusRing %= Focus.focusNext
vtyEventHandler _ e = do
  s <- use id
  case focused s of
    Just ItemListName -> zoom itemList (handleListEventVi handleListEvent e)
    Just PendingListName ->
      zoom pendingList (handleListEventVi handleListEvent e)
    _ -> pure ()

internalEventHandler
  :: BChan HocketEvent
  -> HocketEvent
  -> EventM Name HocketState ()
internalEventHandler es (HocketAsync e) = asyncCommandEventHandler es e
internalEventHandler es (HocketUi e) = uiCommandEventHandler es e

unlessAsyncRunning :: MonadState HocketState m => m () -> m ()
unlessAsyncRunning act = do
  asyncRunning <- use id <&> view hsAsync <&> isJust
  unless asyncRunning act

asyncCommandEventHandler
  :: BChan HocketEvent
  -> AsyncCommand
  -> EventM Name HocketState ()
asyncCommandEventHandler es FetchItems = do
  s <- use id
  unlessAsyncRunning $ do
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
    hsAsync ?= fetchAsync
asyncCommandEventHandler _ (FetchedItems ts pis) = do
  let (newItems, toBeDeleted) = partition ((== Normal) . view status) pis
  id %= insertItems newItems
  id %= removeItems (toBeDeleted ^.. each . itemId)
  hsAsync .= Nothing
  hsLastUpdated ?= ts

asyncCommandEventHandler es (AsyncActionFailed err) = do
  liftIO (es `trigger` setStatusEvt (Just ("failed" <> maybe "" (": " <>) err)))
  hsAsync .= Nothing
asyncCommandEventHandler es ArchiveItems = do
  s <- use id
  case hsNumItems s of
    (_, 0) -> pure ()
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
      hsAsync ?= archiveAsync
asyncCommandEventHandler _ (ArchivedItems pis) = do
  id %= removeItems pis
  hsAsync .= Nothing
asyncCommandEventHandler es (GetRedditCommentCount items) = unlessAsyncRunning $ do
  liftIO $ es `trigger` setStatusEvt (Just "getting comment counts")
  let maybeSubredditsAndIds = mapMaybe (\i -> (view itemId i,) <$> subredditAndArticleId i) items
  asyncGet <- liftIO . async $ do
    forConcurrently_ maybeSubredditsAndIds $ \(pid, (subreddit, articleId)) -> do
      eitherErrCount <- tryHttpException $ fetchRedditCommentCount subreddit articleId
      for_ eitherErrCount $ \count -> es `trigger` gotRedditCommentsEvt pid count
      es `trigger` doneWithRedditCommentsEvt
    es `trigger` setStatusEvt Nothing
  hsAsync ?= asyncGet
asyncCommandEventHandler _ (GotRedditCommentCount pid (RedditCommentCount count)) = do
  hsContents . at pid . _Just . _2 . redditCommentCount ?= count
asyncCommandEventHandler _ DoneWithRedditComments =
  hsAsync .= Nothing

uiCommandEventHandler :: BChan HocketEvent
                      -> UiCommand
                      -> EventM Name HocketState ()
uiCommandEventHandler _ (ShiftItem pid) = id %= toggleStatus pid
uiCommandEventHandler _ (RemoveItems pis) = id %= removeItems pis
uiCommandEventHandler _ (SetStatus t) = hsStatus .= t
uiCommandEventHandler es (BrowseItem pit) = do
  res <- liftIO . try @SomeException $ browseItem "firefox '%s'" (resolvedOrGivenUrl pit)
  case res of
    Left e -> liftIO $ es `trigger` setStatusEvt (Just (T.pack $ show e))
    Right () -> pure ()

myEventHandler
  :: BChan HocketEvent
  -> BrickEvent Name HocketEvent
  -> EventM Name HocketState ()
myEventHandler es (VtyEvent e) = vtyEventHandler es e
myEventHandler es (AppEvent e) = internalEventHandler es e
myEventHandler _ _ = pure ()

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
  , appHandleEvent = \e -> do
      myEventHandler events e
      id %= syncForRender
  , appStartEvent = liftIO (events `trigger` fetchItemsEvt)
  , appAttrMap = const hocketAttrMap
  }

hocketAttrMap :: AttrMap
hocketAttrMap =
  attrMap
    Vty.defAttr
    [ (attrName "list" <> attrName "selected" <> attrName "focused", boldBlackOnOrange)
    , (attrName "list" <> attrName "unselectedItem", whiteFg)
    , ( attrName "bar"
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
            (attrName "bar")
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
    Just ItemListName -> s ^? itemList
    Just PendingListName -> s ^? pendingList
    _ -> Nothing

isFocused :: HocketState -> Name -> Bool
isFocused s name = Just name == focused s

listDrawElement :: Bool -> PocketItem -> Widget Name
listDrawElement sel e =
  (if sel
     then withAttr (attrName "list" <> attrName "selectedItem")
     else withAttr (attrName "list" <> attrName "unselectedItem"))
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
hBar = withAttr (attrName "bar") . padRight Max . txt

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
       (find (not . T.null) [given, resolved, T.pack url])) <+>
  padLeft Max (hLimit horizontalUriLimit (txt trimmedUrl)) <+>
  txt commentCount
  where
    resolved = view resolvedTitle pit
    given = view givenTitle pit
    (URL url) = resolvedOrGivenUrl pit
    added = posixSecondsToUTCTime (view timeAdded pit)
    leftEdge =
      sformat (F.year % "-" <> F.month % "-" <> F.dayOfMonth) added <> ": "
    commentCount =
      maybe "     " (sformat ("  " % (F.left 3 ' ' %. F.int) % "")) (pit ^. redditCommentCount)
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
