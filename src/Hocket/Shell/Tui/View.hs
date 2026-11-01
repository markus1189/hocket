{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hocket.Shell.Tui.View
  ( drawGui,
    hocketAttrMap,
  )
where

import Brick
  ( AttrMap,
    Padding (Max),
    Widget,
    attrMap,
    attrName,
    emptyWidget,
    hLimit,
    padLeft,
    padRight,
    txt,
    vBox,
    withAttr,
    (<+>),
  )
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Edit (editFocusedAttr, renderEditor)
import qualified Brick.Widgets.List as L (renderList)
import Control.Lens (view)
import Control.Lens.Operators ((^.))
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
  ( concat,
    empty,
    justifyRight,
    null,
    pack,
    unpack,
  )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import Formatting (sformat, (%))
import qualified Formatting as F (int)
import qualified Formatting.Time as F (hms)
import qualified Graphics.Vty as Vty
  ( Attr,
    Color,
    black,
    bold,
    defAttr,
    red,
    rgbColor,
    white,
    withBackColor,
    withForeColor,
    withStyle,
  )
import Hocket.App.Model
  ( Model,
    Name,
    VideoFilterMode (HideVideos, NoVideoFilter, ShowOnlyVideos),
    focusedItem,
    getPendingActionForItem,
    icFutureReminders,
    icNone,
    icReminderToBeRemoved,
    icToBeArchived,
    icToBeReminded,
    mAgentClients,
    mAgentError,
    mFilterActive,
    mFilterQuery,
    mLastUpdated,
    mNumItems,
    mStatus,
    mVideoFilter,
  )
import Hocket.Domain.Bookmark
  ( BookmarkItem,
    PendingAction
      ( None,
        ReminderToBeRemoved,
        ToBeArchived,
        ToBeReminded
      ),
    biCreated,
    biExcerpt,
    biId,
    biImportant,
    biLink,
    biNote,
    biReminder,
    biTitle,
  )
import Hocket.Domain.Filter (sanitizeForDisplay)
import Hocket.Shell.Tui.Reconcile (UiState, uiEditor, uiList)
import Network.URI
  ( URI (uriAuthority, uriPath, uriQuery),
    URIAuth (uriRegName),
    parseURI,
  )

hocketAttrMap :: AttrMap
hocketAttrMap =
  attrMap
    Vty.defAttr
    [ (attrName "list" <> attrName "selected" <> attrName "focused", boldBlackOnOrange),
      (attrName "list" <> attrName "listSelected", Vty.defAttr `Vty.withStyle` Vty.bold),
      (attrName "list" <> attrName "unselectedItem", whiteFg),
      (attrName "list" <> attrName "flaggedItem", flaggedRedFg),
      (attrName "list" <> attrName "flaggedSelected", flaggedRedSelectedFg),
      (attrName "list" <> attrName "reminderItem", reminderBlueFg),
      (attrName "list" <> attrName "reminderSelected", reminderBlueSelectedFg),
      (attrName "list" <> attrName "reminderFlaggedItem", reminderFlaggedFg),
      (attrName "list" <> attrName "reminderFlaggedSelected", reminderFlaggedSelectedFg),
      (attrName "list" <> attrName "reminderRemovalItem", reminderRemovalFg),
      (attrName "list" <> attrName "reminderRemovalSelected", reminderRemovalSelectedFg),
      (attrName "list" <> attrName "favoriteItem", favoriteYellowFg),
      (attrName "list" <> attrName "favoriteSelected", favoriteYellowSelectedFg),
      (attrName "bar", barAttr),
      (attrName "bar" <> attrName "fixture", fixtureAttr),
      -- The editor sets its own attr, which would drop the bar's colours.
      (editFocusedAttr, barAttr)
    ]

-- | Loud on purpose: a fixture session must not pass for the real account.
fixtureAttr :: Vty.Attr
fixtureAttr = Vty.defAttr `Vty.withBackColor` flaggedRed `Vty.withForeColor` Vty.white `Vty.withStyle` Vty.bold

barAttr :: Vty.Attr
barAttr = Vty.defAttr `Vty.withBackColor` Vty.black `Vty.withForeColor` Vty.white

getDisplayContent :: BookmarkItem -> Text
getDisplayContent item =
  let noteText = item ^. biNote
      excerptText = item ^. biExcerpt
      reminderDate = item ^. biReminder
      hasNote = not (T.null noteText)
      hasExcerpt = not (T.null excerptText)
      hasReminder = isJust reminderDate
      formattedNote = if hasNote then "NOTE " <> sanitizeForDisplay noteText else T.empty
      formattedReminder = case reminderDate of
        Just reminder -> "REMINDER " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" reminder) <> " "
        Nothing -> T.empty
      formattedExcerpt = if hasExcerpt then "EXCERPT " <> sanitizeForDisplay excerptText else T.empty
   in case (hasNote, hasReminder, hasExcerpt) of
        (True, True, True) -> formattedNote <> " | " <> formattedReminder <> formattedExcerpt
        (True, True, False) -> formattedNote <> " | " <> formattedReminder
        (True, False, True) -> formattedNote <> " | " <> formattedExcerpt
        (True, False, False) -> formattedNote
        (False, True, True) -> formattedReminder <> formattedExcerpt
        (False, True, False) -> formattedReminder
        (False, False, True) -> formattedExcerpt
        (False, False, False) -> " "

drawGui :: TimeZone -> Bool -> Model -> UiState -> [Widget Name]
drawGui tz isFixture s u = [w]
  where
    fixtureTag = if isFixture then withAttr (attrName "bar" <> attrName "fixture") (txt " FIXTURE ") else emptyWidget
    w =
      vBox
        [ fixtureTag
            <+> hBarWithHints
              ( ( case (s ^. mAgentError, s ^. mAgentClients > 0) of
                    (Just _, _) -> "[agent: socket failed] "
                    (Nothing, True) -> "[agent] "
                    (Nothing, False) -> ""
                )
                  <> "Hocket"
                  <> ( case s ^. mVideoFilter of
                         NoVideoFilter -> ""
                         ShowOnlyVideos -> " (+V)"
                         HideVideos -> " (-V)"
                     )
                  <> ( if T.null (s ^. mFilterQuery)
                         then ""
                         else " (/" <> sanitizeForDisplay (s ^. mFilterQuery) <> ")"
                     )
                  <> ": "
                  <> ( \counts ->
                         let base =
                               sformat
                                 ("(" % F.int % "|" % F.int % ")")
                                 (counts ^. icNone)
                                 (counts ^. icToBeArchived + counts ^. icToBeReminded + counts ^. icReminderToBeRemoved)
                             reminderPart =
                               if counts ^. icFutureReminders > 0
                                 then sformat (" (" % F.int % ")") (counts ^. icFutureReminders)
                                 else ""
                          in base <> reminderPart
                     )
                    (mNumItems s)
              )
              "spc:Browse ent:Browse+flag e:Edit y:Copied URL r:Refresh S:Toggle future reminders v:Video filter V:Hide videos /:Filter X:Execute Flags a:Archive flag s:Reminder flag u:Unflag J/K:Jump U:Unflag all Q:Quit",
          hBorder,
          hBar
            ( maybe
                " "
                getDisplayContent
                (focusedItem s)
            ),
          hBorder,
          L.renderList
            (listDrawElementWithAction s)
            True
            (u ^. uiList),
          hBar " "
            <+> withAttr
              (attrName "bar")
              ( padLeft
                  Max
                  ( txt
                      ( maybe
                          "<never>"
                          (sformat F.hms . utcToLocalTime tz . posixSecondsToUTCTime)
                          (s ^. mLastUpdated)
                      )
                  )
              ),
          if s ^. mFilterActive
            then
              withAttr (attrName "bar") . padRight Max $
                txt "/" <+> renderEditor (txt . sanitizeForDisplay . T.concat) True (u ^. uiEditor)
            else txt (fromMaybe " " (s ^. mStatus))
        ]

listDrawElementWithAction :: Model -> Bool -> BookmarkItem -> Widget Name
listDrawElementWithAction s sel e =
  let actionIndicator = case getPendingActionForItem (view biId e) s of
        ToBeArchived -> "A "
        ToBeReminded _ -> "R "
        ReminderToBeRemoved -> "r "
        None -> "  "
      pendingAction = getPendingActionForItem (view biId e) s
      hasReminder = isJust (view biReminder e)
      isFavorite = view biImportant e
      attrName' = case (pendingAction, hasReminder, isFavorite, sel) of
        (ToBeArchived, _, _, True) -> attrName "list" <> attrName "flaggedSelected"
        (ToBeArchived, _, _, False) -> attrName "list" <> attrName "flaggedItem"
        (ToBeReminded _, _, _, True) -> attrName "list" <> attrName "reminderFlaggedSelected"
        (ToBeReminded _, _, _, False) -> attrName "list" <> attrName "reminderFlaggedItem"
        (ReminderToBeRemoved, _, _, True) -> attrName "list" <> attrName "reminderRemovalSelected"
        (ReminderToBeRemoved, _, _, False) -> attrName "list" <> attrName "reminderRemovalItem"
        (None, True, _, True) -> attrName "list" <> attrName "reminderSelected"
        (None, True, _, False) -> attrName "list" <> attrName "reminderItem"
        (None, False, True, True) -> attrName "list" <> attrName "favoriteSelected"
        (None, False, True, False) -> attrName "list" <> attrName "favoriteItem"
        (None, False, False, True) -> attrName "list" <> attrName "listSelected"
        (None, False, False, False) -> attrName "list" <> attrName "unselectedItem"
   in withAttr attrName' (txt actionIndicator <+> padRight Max (txtDisplay e))

orange :: Vty.Color
orange = Vty.rgbColor 215 135 (0 :: Int)

boldBlackOnOrange :: Vty.Attr
boldBlackOnOrange =
  Vty.defAttr
    `Vty.withForeColor` black
    `Vty.withBackColor` orange
    `Vty.withStyle` Vty.bold

black :: Vty.Color
black = Vty.rgbColor zero zero zero
  where
    zero = 0 :: Int

flaggedRed :: Vty.Color
flaggedRed = Vty.rgbColor (220 :: Int) (85 :: Int) (85 :: Int)

reminderBlue :: Vty.Color
reminderBlue = Vty.rgbColor (100 :: Int) (150 :: Int) (200 :: Int)

favoriteYellow :: Vty.Color
favoriteYellow = Vty.rgbColor (200 :: Int) (180 :: Int) (100 :: Int)

whiteFg :: Vty.Attr
whiteFg = Vty.defAttr `Vty.withForeColor` Vty.white

flaggedRedFg :: Vty.Attr
flaggedRedFg = Vty.defAttr `Vty.withForeColor` flaggedRed

flaggedRedSelectedFg :: Vty.Attr
flaggedRedSelectedFg = boldBlackOnOrange

reminderBlueFg :: Vty.Attr
reminderBlueFg = Vty.defAttr `Vty.withForeColor` reminderBlue

reminderBlueSelectedFg :: Vty.Attr
reminderBlueSelectedFg = boldBlackOnOrange

reminderFlaggedFg :: Vty.Attr
reminderFlaggedFg = Vty.defAttr `Vty.withForeColor` reminderBlue

reminderFlaggedSelectedFg :: Vty.Attr
reminderFlaggedSelectedFg = boldBlackOnOrange

reminderRemovalFg :: Vty.Attr
reminderRemovalFg = Vty.defAttr `Vty.withForeColor` Vty.red

reminderRemovalSelectedFg :: Vty.Attr
reminderRemovalSelectedFg = boldBlackOnOrange

favoriteYellowFg :: Vty.Attr
favoriteYellowFg = Vty.defAttr `Vty.withForeColor` favoriteYellow

favoriteYellowSelectedFg :: Vty.Attr
favoriteYellowSelectedFg = boldBlackOnOrange

hBar :: Text -> Widget Name
hBar = withAttr (attrName "bar") . padRight Max . txt

hBarWithHints :: Text -> Text -> Widget Name
hBarWithHints leftText rightText =
  withAttr (attrName "bar") (txt leftText <+> padLeft Max (txt rightText))

txtDisplay :: BookmarkItem -> Widget Name
txtDisplay bit =
  txt (T.justifyRight 10 ' ' leftEdge)
    <+> txt favoriteIndicator
    <+> txt
      ( sanitizeForDisplay $
          fromMaybe
            "<empty>"
            (find (not . T.null) [view biTitle bit, T.pack url])
      )
    <+> padLeft Max (hLimit horizontalUriLimit (txt trimmedUrl))
  where
    url = T.unpack (view biLink bit)
    added = view biCreated bit
    reminderDate = view biReminder bit
    favoriteIndicator = if view biImportant bit then "★" else " "
    leftEdge = case reminderDate of
      Just reminder -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" reminder <> ":")
      Nothing -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" added <> ":")
    trimmedUrl = T.pack (trimURI url)

horizontalUriLimit :: Int
horizontalUriLimit = 60

trimURI :: String -> String
trimURI uri =
  fromMaybe uri $ do
    parsed <- parseURI uri
    auth <- uriAuthority parsed
    return
      ( strip
          "reddit.com/"
          (strip "www." (uriRegName auth) <> uriPath parsed <> uriQuery parsed)
      )
  where
    strip prefix s =
      if prefix `isPrefixOf` s
        then drop (length prefix) s
        else s
