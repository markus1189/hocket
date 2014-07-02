{-# LANGUAGE OverloadedStrings #-}
module Printing ( newestFirst
                ) where

import           Data.Foldable (traverse_)
import           Data.List (sortBy)
import           Data.Ord (comparing)
import qualified Data.Text.IO as TIO

import           Parsing

newestFirst :: [PocketItem] -> IO ()
newestFirst = traverse_ printItem . reverseSortBy (comparing timeAdded)
  where reverseSortBy p = sortBy (flip p)
        printItem itm = do
          TIO.putStr $ itemId itm
          TIO.putStr "|"
          TIO.putStr $ resolvedTitle itm
          TIO.putStr "|"
