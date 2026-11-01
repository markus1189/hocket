-- | List navigation by id, as brick-2.9's list moves it by index
-- ('Brick.Widgets.List.listMoveBy', 'listMoveByPages'), plus J/K.
module Network.Bookmark.App.Nav (Nav (..), navigate) where

import Control.Lens ((^.))
import qualified Data.Vector as V
import Network.Bookmark.Types (BookmarkItem, BookmarkItemId, PendingAction (None), biId)

-- | Counts are signed: @Page (-1)@ is PageUp, @HalfPage 1@ is Ctrl-d.
data Nav = Up | Down | Top | Bottom | Page !Int | HalfPage !Int | NextFlagged | PrevFlagged
  deriving (Show, Eq)

-- | The selection after a move, for a viewport of the given height in rows
-- (one row per item). A selected id missing from the list counts as no
-- selection. 'Nothing' only for an empty list, or when J/K start from no
-- selection, which they leave unchanged.
navigate :: Int -> Nav -> V.Vector BookmarkItem -> (BookmarkItemId -> PendingAction) -> Maybe BookmarkItemId -> Maybe BookmarkItemId
navigate height nav items flagOf selected =
  idAt <$> case nav of
    Up -> moveBy (-1)
    Down -> moveBy 1
    Top -> moveTo 0
    Bottom -> moveTo (len - 1)
    Page n -> moveBy (pages (fromIntegral n))
    HalfPage n -> moveBy (pages (0.5 * fromIntegral n))
    NextFlagged -> current >>= \i -> Just (maybe i (+ (i + 1)) (V.findIndex flagged (V.drop (i + 1) items)))
    PrevFlagged -> current >>= \i -> Just (maybe i (\k -> i - 1 - k) (V.findIndex flagged (V.reverse (V.take i items))))
  where
    len = V.length items
    current = selected >>= \bid -> V.findIndex ((== bid) . (^. biId)) items
    idAt i = (items V.! i) ^. biId
    flagged item = flagOf (item ^. biId) /= None
    moveTo i
      | len == 0 = Nothing
      | otherwise = Just (max 0 (min (len - 1) i))
    -- listMoveBy: from no selection, forward starts at the top, anything
    -- else (zero included) at the bottom.
    moveBy amt = moveTo $ case current of
      Nothing
        | amt > 0 -> 0
        | otherwise -> len - 1
      Just i -> max 0 (i + amt)
    -- listMoveByPages with an item height of 1; 'round' is banker's.
    pages :: Double -> Int
    pages p = round (p * fromIntegral height)
