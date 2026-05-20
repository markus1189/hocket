module Network.Bookmark.Ui.Widgets
  ( listRemoveSelected,
    listInsertSorted,
    clamp,
    sanitizeForDisplay,
  )
where

import Brick.Widgets.List (List)
import qualified Brick.Widgets.List as L
import Control.Lens
import Data.Char (isControl)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

listRemoveSelected :: List n e -> (Maybe e, List n e)
listRemoveSelected l = fromMaybe (Nothing, l) $ do
  sel <- l ^. L.listSelectedL
  let es = l ^. L.listElementsL
      newSel = clamp 0 (V.length es - 2) sel
      newEs = V.take sel es <> V.drop (sel + 1) es
  return
    ( es V.!? sel,
      l
        & L.listElementsL .~ newEs
        & L.listSelectedL ?~ newSel
    )

listInsertSorted :: (Ord b) => (a -> b) -> a -> L.List n a -> L.List n a
listInsertSorted toOrd x lxs = L.listInsert insertPos x lxs
  where
    insertPos :: Int
    insertPos =
      fromMaybe
        (length xs)
        (V.findIndex (((<) `on` toOrd) x) xs)
    xs = L.listElements lxs

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx val = max mn (min val mx)

-- Make a Text safe for single-line Vty rendering. Three problems get
-- handled here:
--
--   * Control characters (C0, DEL, C1) become spaces so they don't emit
--     terminal escapes or break list-row alignment.
--   * Composing codepoints (ZWJ, Variation Selectors, Emoji skin-tone
--     modifiers) are dropped: the terminal merges them into the preceding
--     glyph but Vty's wcwidth counts them separately.
--   * Codepoints in modern emoji ranges are replaced with two spaces.
--     Vty's wcwidth table reports them as width 1 while terminals render
--     them as width 2, so leaving them in shifts the right-hand columns
--     of every list row. Two spaces preserves the visual gap and keeps
--     Vty's accounting in sync with the terminal.
sanitizeForDisplay :: Text -> Text
sanitizeForDisplay = T.concatMap replaceChar . T.filter (not . isComposing)
  where
    replaceChar c
      | isControl c = T.singleton ' '
      | isAssumedWide c = T.pack "  "
      | otherwise = T.singleton c
    isComposing c =
      c == '\x200D'
        || (c >= '\xFE00' && c <= '\xFE0F')
        || (c >= '\x1F3FB' && c <= '\x1F3FF')
    isAssumedWide c =
      (c >= '\x2600' && c <= '\x27BF')
        || (c >= '\x1F000' && c <= '\x1FFFF')
