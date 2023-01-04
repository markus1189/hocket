{-# LANGUAGE OverloadedStrings #-}

module Network.Pocket.Meta where

import           Control.Lens ((^?))
import           Data.Aeson.Lens
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Formatting (sformat, string, (%))
import           Network.Pocket.Types
import           Network.Wreq (get, responseBody)

fetchRedditCommentCount :: String -> String -> IO RedditCommentCount
fetchRedditCommentCount subreddit articleId = do
  r <- get (T.unpack (sformat url subreddit articleId))
  let n = r ^? responseBody . nth 0 . key "data" . key "children" . nth 0 . key "data" . key "num_comments" . _Integer
  return (RedditCommentCount $ fromMaybe 0 n)
  where
    url = "https://www.reddit.com/r/" % string % "/comments/" % string % ".json"
