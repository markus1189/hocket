{-# LANGUAGE OverloadedStrings #-}

module SystemTests (systemTests) where

import Control.Exception (throwIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Interpret (copyCleaned)
import Network.Bookmark.App.Msg (Msg (..))
import System (cleanUrl, urlReplacements)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

systemTests :: TestTree
systemTests = testGroup "system" [cleanUrlTests, copyTests]

-- | The y key's path from CopyText to the clipboard, with the clipboard faked.
copyTests :: TestTree
copyTests =
  testGroup
    "copy"
    [ testCase "the cleaned URL is copied and reported" $ do
        copied <- newIORef ""
        msgs <- copyCleaned (writeIORef copied) "https://m.imdb.com/title/tt0111161/"
        readIORef copied >>= (@?= "https://imdb.com/title/tt0111161/")
        msgs @?= [Copied "https://imdb.com/title/tt0111161/"],
      testCase "a failed copy is reported" $ do
        msgs <- copyCleaned (const (throwIO (userError "no xclip"))) "https://example.org/"
        msgs @?= [SystemFailed "Copy failed: user error (no xclip)"]
    ]

cleanUrlTests :: TestTree
cleanUrlTests =
  testGroup
    "cleanUrl"
    [ testCase "the table has a row for every replacement" $
        length urlReplacements @?= 2,
      testCase "m.imdb. becomes imdb." $
        cleanUrl "https://m.imdb.com/title/tt0111161/" @?= "https://imdb.com/title/tt0111161/",
      testCase "m.aliexpress. becomes aliexpress." $
        cleanUrl "https://m.aliexpress.com/item/1.html" @?= "https://aliexpress.com/item/1.html",
      testCase "a URL no replacement matches is unchanged" $
        cleanUrl "https://example.org/m.path?q=imdb" @?= "https://example.org/m.path?q=imdb",
      testCase "several replacements apply in one URL" $
        cleanUrl "https://m.imdb.com/?ref=https://m.aliexpress.com/x" @?= "https://imdb.com/?ref=https://aliexpress.com/x"
    ]
