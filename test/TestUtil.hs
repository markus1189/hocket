{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestUtil
  ( bookmarkItem1,
    testState,
  )
where

import Hocket.App.Model
import Hocket.Domain.Bookmark

bookmarkItem1 :: BookmarkItem
bookmarkItem1 =
  BookmarkItem
    (BookmarkItemId "1")
    "www.url.com"
    "excerpt"
    "note"
    "link"
    []
    False
    (read "2016-05-22 12:33:11 UTC")
    (read "2016-05-22 12:33:11 UTC")
    "url.com"
    "given title"
    0
    []
    0
    False
    Nothing

testState = initialModel
