{-# LANGUAGE OverloadedStrings #-}
module Printing ( newestFirst
                ) where

import           Control.Lens (view)
import           Data.Foldable (traverse_)
import           Data.List (sortBy)
import           Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Network.Pocket

newestFirst :: [PocketItem] -> IO ()
newestFirst = traverse_ printItem . reverseSortBy (comparing $ view timeAdded)
  where reverseSortBy p = sortBy (flip p)
        printItem itm = do
          TIO.putStr . T.pack . show . view itemId $ itm
          TIO.putStr "|"
          TIO.putStr $ view resolvedTitle itm
          TIO.putStr "|"
