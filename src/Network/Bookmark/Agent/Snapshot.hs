{-# LANGUAGE OverloadedStrings #-}

-- | A serializable mirror of 'HocketState' for the agent control socket.
-- Deliberately excludes credentials: the Raindrop token structurally cannot
-- cross the socket because it is not part of this type.
module Network.Bookmark.Agent.Snapshot
  ( AgentSnapshot (..),
    SnapshotItem (..),
    takeSnapshot,
    emptySnapshot,
    itemIdText,
    videoFilterModeToText,
    asyncOpToText,
    pendingActionToJSON,
  )
where

import qualified Brick.Widgets.List as L
import Control.Lens (view, (^.))
import Data.Aeson (ToJSON (toJSON), Value, object, (.=))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Vector as V
import Network.Bookmark.Types
  ( BookmarkItem,
    BookmarkItemId (..),
    PendingAction (..),
    biCreated,
    biExcerpt,
    biId,
    biImportant,
    biLink,
    biNote,
    biReminder,
    biTags,
    biTitle,
  )
import Network.Bookmark.Ui.State
  ( AsyncOp (..),
    HocketState,
    VideoFilterMode (..),
    hsAsyncOp,
    hsContents,
    hsFilterQuery,
    hsLastUpdated,
    hsShowFutureReminders,
    hsStatus,
    hsVideoFilter,
    itemList,
  )

data SnapshotItem = SnapshotItem
  { siId :: !BookmarkItemId,
    siTitle :: !Text,
    siLink :: !Text,
    siTags :: ![Text],
    siNote :: !Text,
    siExcerpt :: !Text,
    siCreated :: !UTCTime,
    siReminder :: !(Maybe UTCTime),
    siFavorite :: !Bool,
    siPending :: !PendingAction,
    siVisible :: !Bool
  }
  deriving (Show, Eq)

data AgentSnapshot = AgentSnapshot
  { asVersion :: !Int,
    asItems :: ![SnapshotItem],
    asSelected :: !(Maybe BookmarkItemId),
    asFilterQuery :: !Text,
    asVideoFilter :: !VideoFilterMode,
    asShowFutureReminders :: !Bool,
    asStatus :: !(Maybe Text),
    asLastUpdated :: !(Maybe POSIXTime),
    asAsyncOp :: !(Maybe AsyncOp)
  }
  deriving (Show, Eq)

itemIdText :: BookmarkItemId -> Text
itemIdText (BookmarkItemId t) = t

videoFilterModeToText :: VideoFilterMode -> Text
videoFilterModeToText NoVideoFilter = "none"
videoFilterModeToText ShowOnlyVideos = "only_videos"
videoFilterModeToText HideVideos = "hide_videos"

asyncOpToText :: AsyncOp -> Text
asyncOpToText OpFetchItems = "fetch"
asyncOpToText OpExecuteBatch = "execute_batch"

pendingActionToJSON :: PendingAction -> Value
pendingActionToJSON None = object ["action" .= ("none" :: Text)]
pendingActionToJSON ToBeArchived = object ["action" .= ("archive" :: Text)]
pendingActionToJSON (ToBeReminded t) = object ["action" .= ("reminder" :: Text), "at" .= t]
pendingActionToJSON ReminderToBeRemoved = object ["action" .= ("remove_reminder" :: Text)]

instance ToJSON SnapshotItem where
  toJSON si =
    object
      [ "id" .= itemIdText (siId si),
        "title" .= siTitle si,
        "link" .= siLink si,
        "tags" .= siTags si,
        "note" .= siNote si,
        "excerpt" .= siExcerpt si,
        "created" .= siCreated si,
        "reminder" .= siReminder si,
        "favorite" .= siFavorite si,
        "pending" .= pendingActionToJSON (siPending si),
        "visible" .= siVisible si
      ]

instance ToJSON AgentSnapshot where
  toJSON snap =
    object
      [ "version" .= asVersion snap,
        "items" .= asItems snap,
        "selected" .= fmap itemIdText (asSelected snap),
        "filter_query" .= asFilterQuery snap,
        "video_filter" .= videoFilterModeToText (asVideoFilter snap),
        "show_future_reminders" .= asShowFutureReminders snap,
        "status" .= asStatus snap,
        "last_updated" .= asLastUpdated snap,
        "async_op" .= fmap asyncOpToText (asAsyncOp snap)
      ]

-- | Project the given state into a snapshot carrying the given version.
-- Visible items (those surviving the filter pipeline, in display order) come
-- first, followed by the remaining hidden items.
takeSnapshot :: Int -> HocketState -> AgentSnapshot
takeSnapshot v s =
  AgentSnapshot
    { asVersion = v,
      asItems = visibleItems <> hiddenItems,
      asSelected = view biId . snd <$> L.listSelectedElement (s ^. itemList),
      asFilterQuery = s ^. hsFilterQuery,
      asVideoFilter = s ^. hsVideoFilter,
      asShowFutureReminders = s ^. hsShowFutureReminders,
      asStatus = s ^. hsStatus,
      asLastUpdated = s ^. hsLastUpdated,
      asAsyncOp = s ^. hsAsyncOp
    }
  where
    visibleBookmarks = V.toList (s ^. itemList . L.listElementsL)
    visibleIdSet = Set.fromList (map (view biId) visibleBookmarks)
    visibleItems =
      mapMaybe
        (\bit -> mkItem True <$> Map.lookup (view biId bit) (s ^. hsContents))
        visibleBookmarks
    hiddenItems =
      [ mkItem False entry
      | (bid, entry) <- Map.toList (s ^. hsContents),
        bid `Set.notMember` visibleIdSet
      ]
    mkItem :: Bool -> (PendingAction, BookmarkItem) -> SnapshotItem
    mkItem vis (action, bit) =
      SnapshotItem
        { siId = view biId bit,
          siTitle = view biTitle bit,
          siLink = view biLink bit,
          siTags = view biTags bit,
          siNote = view biNote bit,
          siExcerpt = view biExcerpt bit,
          siCreated = view biCreated bit,
          siReminder = view biReminder bit,
          siFavorite = view biImportant bit,
          siPending = action,
          siVisible = vis
        }

emptySnapshot :: AgentSnapshot
emptySnapshot =
  AgentSnapshot
    { asVersion = 0,
      asItems = [],
      asSelected = Nothing,
      asFilterQuery = T.empty,
      asVideoFilter = NoVideoFilter,
      asShowFutureReminders = False,
      asStatus = Nothing,
      asLastUpdated = Nothing,
      asAsyncOp = Nothing
    }
