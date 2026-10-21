module Network.Bookmark.Ui.Widgets
  ( listRemoveSelected,
    listInsertSorted,
    clamp,
    sanitizeForDisplay,
    fuzzyMatch,
    fuzzyFilterMatch,
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

-- Case-insensitive fzf-style subsequence match: every char of the query
-- appears in the haystack in order (gaps allowed). Empty query matches all.
fuzzyMatch :: Text -> Text -> Bool
fuzzyMatch query haystack = subsequenceOf (T.unpack (T.toLower query)) (T.toLower haystack)

-- Both arguments must already be lowercased.
subsequenceOf :: String -> Text -> Bool
subsequenceOf [] _ = True
subsequenceOf (q : qs) hs = case T.uncons (T.dropWhile (/= q) hs) of
  Just (_, rest) -> subsequenceOf qs rest
  Nothing -> False

-- Stricter filter match: split the query into whitespace-separated terms;
-- every term must fuzzy-subsequence-match at least one word of the haystack
-- (AND across terms, no matching across word boundaries). An empty /
-- all-whitespace query matches everything.
fuzzyFilterMatch :: Text -> Text -> Bool
fuzzyFilterMatch query = \haystack ->
  let haystackWords = T.words (T.toLower haystack)
   in all (\term -> any (subsequenceOf term) haystackWords) terms
  where
    -- Bound outside the lambda so a partial application
    -- (`filter (fuzzyFilterMatch q . ...)`) prepares the query once, not per item.
    terms = map T.unpack (T.words (T.toLower query))
