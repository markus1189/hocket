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

-- Collapse control characters (C0, DEL, C1) into spaces so they don't
-- emit terminal escapes or break list-row alignment. Also drop codepoints
-- that the terminal merges into the preceding glyph: ZWJ, Variation
-- Selectors, and Emoji Modifiers (skin tones). Vty's wcwidth counts each
-- codepoint separately while the terminal renders the cluster as one
-- cell, so leaving them in shifts every subsequent column.
sanitizeForDisplay :: Text -> Text
sanitizeForDisplay = T.map replaceControl . T.filter (not . isComposing)
  where
    replaceControl c = if isControl c then ' ' else c
    isComposing c =
      c == '\x200D'
        || (c >= '\xFE00' && c <= '\xFE0F')
        || (c >= '\x1F3FB' && c <= '\x1F3FF')
