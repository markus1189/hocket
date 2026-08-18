{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The agent control-socket protocol: newline-delimited JSON requests are
-- parsed into a closed command type at the edge ('decodeCmd'); everything
-- downstream ('serveRead', 'validateWrite') is total and pure. The IO shell
-- that owns the socket lives in the executable.
module Network.Bookmark.Agent.Protocol
  ( AgentCmd (..),
    ReadCmd (..),
    WriteCmd (..),
    FlagAction (..),
    RawRequest (..),
    decodeCmd,
    encodeCmd,
    serveRead,
    validateWrite,
    stateView,
    okResponse,
    errResponse,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    Value (Object),
    object,
    toJSON,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Bookmark.Agent.Snapshot
  ( AgentSnapshot (..),
    SnapshotItem (..),
    asyncOpToText,
    itemIdText,
    videoFilterModeToText,
  )
import Network.Bookmark.Types (BookmarkItemId (..), PendingAction (..))
import Network.Bookmark.Ui.State (VideoFilterMode (..))

data FlagAction
  = FlagArchive
  | FlagReminder
  | FlagRemoveReminder
  | FlagNone
  deriving (Show, Eq)

data ReadCmd
  = CmdGetState
  | -- | visible only / flagged only
    CmdListItems !Bool !Bool
  | CmdGetItem !BookmarkItemId
  deriving (Show, Eq)

data WriteCmd
  = CmdSetFlag !BookmarkItemId !FlagAction
  | CmdClearFlags
  | CmdFlagAllArchive
  | CmdExecute
  | CmdRefresh
  | CmdSetFilter !Text
  | CmdSetVideoFilter !VideoFilterMode
  | CmdSetShowFutureReminders !Bool
  | CmdSelectItem !BookmarkItemId
  | CmdOpenItem !BookmarkItemId
  | CmdSetStatus !Text
  deriving (Show, Eq)

data AgentCmd
  = ARead !ReadCmd
  | AWrite !WriteCmd
  | -- | wait until stateVersion exceeds the first field, timeout ms second
    AWait !Int !Int
  deriving (Show, Eq)

-- | The wire shape of a request; the method/params are decoded separately by
-- 'decodeCmd' so a malformed method can still be answered with the request id.
data RawRequest = RawRequest
  { rawId :: !(Maybe Value),
    rawMethod :: !Text,
    rawParams :: !(Maybe Value)
  }
  deriving (Show, Eq)

instance FromJSON RawRequest where
  parseJSON = withObject "request" $ \o ->
    RawRequest <$> o .:? "id" <*> o .: "method" <*> o .:? "params"

-- | The only place request JSON is inspected: method name plus params to a
-- typed command, or a human-readable error.
decodeCmd :: Text -> Maybe Value -> Either Text AgentCmd
decodeCmd method mparams = case method of
  "get_state" -> Right (ARead CmdGetState)
  "list_items" -> withParams $ \o ->
    fmap ARead (CmdListItems <$> o .:? "visible_only" .!= True <*> o .:? "flagged_only" .!= False)
  "get_item" -> withParams (fmap (ARead . CmdGetItem) . itemIdP)
  "wait_version" -> withParams $ \o ->
    AWait <$> o .: "after" <*> o .:? "timeout_ms" .!= 10000
  "set_flag" -> withParams $ \o -> do
    bid <- itemIdP o
    actTxt <- o .: "action"
    act <- case actTxt :: Text of
      "archive" -> pure FlagArchive
      "reminder" -> pure FlagReminder
      "remove_reminder" -> pure FlagRemoveReminder
      "none" -> pure FlagNone
      other -> fail ("unknown flag action: " <> T.unpack other)
    pure (AWrite (CmdSetFlag bid act))
  "clear_all_flags" -> Right (AWrite CmdClearFlags)
  "flag_all_archive" -> Right (AWrite CmdFlagAllArchive)
  "execute" -> Right (AWrite CmdExecute)
  "refresh" -> Right (AWrite CmdRefresh)
  "set_filter" -> withParams $ \o -> AWrite . CmdSetFilter <$> o .: "query"
  "set_video_filter" -> withParams $ \o -> do
    modeTxt <- o .: "mode"
    mode <- case modeTxt :: Text of
      "none" -> pure NoVideoFilter
      "only_videos" -> pure ShowOnlyVideos
      "hide_videos" -> pure HideVideos
      other -> fail ("unknown video filter mode: " <> T.unpack other)
    pure (AWrite (CmdSetVideoFilter mode))
  "set_show_future_reminders" -> withParams $ \o ->
    AWrite . CmdSetShowFutureReminders <$> o .: "show"
  "select_item" -> withParams (fmap (AWrite . CmdSelectItem) . itemIdP)
  "open_item" -> withParams (fmap (AWrite . CmdOpenItem) . itemIdP)
  "set_status" -> withParams $ \o -> AWrite . CmdSetStatus <$> o .: "text"
  other -> Left ("unknown method: " <> other)
  where
    itemIdP :: Object -> Parser BookmarkItemId
    itemIdP o = BookmarkItemId <$> o .: "id"
    withParams :: (Object -> Parser AgentCmd) -> Either Text AgentCmd
    withParams f = do
      o <- case mparams of
        Nothing -> Right mempty
        Just (Object o) -> Right o
        Just _ -> Left "params must be an object"
      first T.pack (parseEither f o)

-- | Inverse of 'decodeCmd': a typed command back to its wire method name and
-- params object. Pure and total, so a client that builds requests with it
-- cannot produce a method name or enum spelling the server would reject.
-- Round-tripping through 'decodeCmd' is what keeps the shipped CLI and the
-- protocol welded together.
encodeCmd :: AgentCmd -> (Text, Value)
encodeCmd = \case
  ARead CmdGetState -> ("get_state", object [])
  ARead (CmdListItems visOnly flaggedOnly) ->
    ("list_items", object ["visible_only" .= visOnly, "flagged_only" .= flaggedOnly])
  ARead (CmdGetItem bid) -> ("get_item", itemIdParams bid)
  AWait after timeoutMs ->
    ("wait_version", object ["after" .= after, "timeout_ms" .= timeoutMs])
  AWrite (CmdSetFlag bid act) ->
    ("set_flag", object ["id" .= itemIdText bid, "action" .= flagActionToText act])
  AWrite CmdClearFlags -> ("clear_all_flags", object [])
  AWrite CmdFlagAllArchive -> ("flag_all_archive", object [])
  AWrite CmdExecute -> ("execute", object [])
  AWrite CmdRefresh -> ("refresh", object [])
  AWrite (CmdSetFilter q) -> ("set_filter", object ["query" .= q])
  AWrite (CmdSetVideoFilter m) ->
    ("set_video_filter", object ["mode" .= videoFilterModeToText m])
  AWrite (CmdSetShowFutureReminders b) ->
    ("set_show_future_reminders", object ["show" .= b])
  AWrite (CmdSelectItem bid) -> ("select_item", itemIdParams bid)
  AWrite (CmdOpenItem bid) -> ("open_item", itemIdParams bid)
  AWrite (CmdSetStatus t) -> ("set_status", object ["text" .= t])
  where
    itemIdParams bid = object ["id" .= itemIdText bid]

-- | The wire spelling of a flag action, as accepted by 'decodeCmd'.
flagActionToText :: FlagAction -> Text
flagActionToText = \case
  FlagArchive -> "archive"
  FlagReminder -> "reminder"
  FlagRemoveReminder -> "remove_reminder"
  FlagNone -> "none"

-- | Serve a read command purely from the snapshot.
serveRead :: AgentSnapshot -> ReadCmd -> Either Text Value
serveRead snap CmdGetState = Right (stateView snap)
serveRead snap (CmdListItems visOnly flaggedOnly) =
  Right (toJSON (filter keep (asItems snap)))
  where
    keep si =
      (not visOnly || siVisible si)
        && (not flaggedOnly || siPending si /= None)
serveRead snap (CmdGetItem bid) =
  case findItem snap bid of
    Just si -> Right (toJSON si)
    Nothing -> Left (unknownItem bid)

-- | Reject writes that cannot make sense against the given snapshot. The
-- snapshot may be a moment stale, but every check here is on slow-changing
-- item identity/reminder facts, not on cursor-level state.
validateWrite :: AgentSnapshot -> WriteCmd -> Either Text WriteCmd
validateWrite snap cmd = case cmd of
  CmdSetFlag bid act -> do
    si <- lookupItem bid
    case (act, siReminder si) of
      (FlagRemoveReminder, Nothing) ->
        Left ("item has no reminder to remove: " <> itemIdText bid)
      (FlagReminder, Just _) ->
        Left ("item already has a reminder (flag remove_reminder instead): " <> itemIdText bid)
      _ -> Right cmd
  CmdSelectItem bid -> do
    si <- lookupItem bid
    if siVisible si
      then Right cmd
      else Left ("item is not visible under the current filters: " <> itemIdText bid)
  CmdOpenItem bid -> lookupItem bid >> Right cmd
  _ -> Right cmd
  where
    lookupItem bid = maybe (Left (unknownItem bid)) Right (findItem snap bid)

findItem :: AgentSnapshot -> BookmarkItemId -> Maybe SnapshotItem
findItem snap bid = find ((== bid) . siId) (asItems snap)

unknownItem :: BookmarkItemId -> Text
unknownItem bid = "unknown item id: " <> itemIdText bid

-- | The compact header view served by get_state and wait_version.
stateView :: AgentSnapshot -> Value
stateView snap =
  object
    [ "version" .= asVersion snap,
      "counts"
        .= object
          [ "total" .= length items,
            "visible" .= length (filter siVisible items),
            "archive_flagged" .= countPending (== ToBeArchived),
            "reminder_flagged" .= countPending isToBeReminded,
            "remove_reminder_flagged" .= countPending (== ReminderToBeRemoved)
          ],
      "selected" .= fmap itemIdText (asSelected snap),
      "filter_query" .= asFilterQuery snap,
      "video_filter" .= videoFilterModeToText (asVideoFilter snap),
      "show_future_reminders" .= asShowFutureReminders snap,
      "status" .= asStatus snap,
      "last_updated" .= asLastUpdated snap,
      "async_op" .= fmap asyncOpToText (asAsyncOp snap)
    ]
  where
    items = asItems snap
    countPending p = length (filter (p . siPending) items)
    isToBeReminded a = case a of
      ToBeReminded _ -> True
      _ -> False

okResponse :: Maybe Value -> Value -> Value
okResponse mid result = object ["id" .= mid, "ok" .= True, "result" .= result]

errResponse :: Maybe Value -> Text -> Value
errResponse mid msg = object ["id" .= mid, "ok" .= False, "error" .= msg]
