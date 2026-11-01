{-# LANGUAGE OverloadedStrings #-}

-- | A serializable mirror of 'Model' for the agent control socket.
-- Deliberately excludes credentials: the Raindrop token structurally cannot
-- cross the socket because it is not part of this type.
module Hocket.Agent.Snapshot
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
import Hocket.App.Model
  ( AsyncOp (..),
    Model,
    VideoFilterMode (..),
    mAsyncOp,
    mContents,
    mFilterQuery,
    mLastUpdated,
    mSelected,
    mShowFutureReminders,
    mStatus,
    mVideoFilter,
    mVisible,
  )
import Hocket.Domain.Bookmark
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
takeSnapshot :: Int -> Model -> AgentSnapshot
takeSnapshot v s =
  AgentSnapshot
    { asVersion = v,
      asItems = visibleItems <> hiddenItems,
      asSelected = s ^. mSelected,
      asFilterQuery = s ^. mFilterQuery,
      asVideoFilter = s ^. mVideoFilter,
      asShowFutureReminders = s ^. mShowFutureReminders,
      asStatus = s ^. mStatus,
      asLastUpdated = s ^. mLastUpdated,
      asAsyncOp = s ^. mAsyncOp
    }
  where
    visibleBookmarks = V.toList (s ^. mVisible)
    visibleIdSet = Set.fromList (map (view biId) visibleBookmarks)
    visibleItems =
      mapMaybe
        (\bit -> mkItem True <$> Map.lookup (view biId bit) (s ^. mContents))
        visibleBookmarks
    hiddenItems =
      [ mkItem False entry
      | (bid, entry) <- Map.toList (s ^. mContents),
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
