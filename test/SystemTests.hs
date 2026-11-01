{-# LANGUAGE OverloadedStrings #-}

module SystemTests (systemTests) where

import Brick.BChan (newBChan)
import Control.Exception (throwIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Time (utc)
import Interpret (Env (..), copyCleaned, interpret)
import Network.Bookmark.App.Effect (Effect (..), Opener (..))
import Network.Bookmark.App.Msg (Msg (..), Now (..))
import Network.Bookmark.Port (RaindropPort (..), SystemPort (..))
import Network.Bookmark.Types (URL (..))
import System (cleanUrl, urlReplacements)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

systemTests :: TestTree
systemTests = testGroup "system" [cleanUrlTests, copyTests, systemPortTests]

-- | interpret reaches the desktop only through the SystemPort.
systemPortTests :: TestTree
systemPortTests =
  testGroup
    "SystemPort"
    [ testCase "OpenUrl hands the program and the URL to sysOpenUrl" $ do
        (env, calls) <- recordingEnv
        msgs <- interpret env (OpenUrl Firefox "https://m.imdb.com/title/tt0111161/")
        msgs @?= []
        readIORef calls >>= (@?= ["open firefox https://m.imdb.com/title/tt0111161/"]),
      testCase "a browser that fails is reported" $ do
        (env, _) <- recordingEnv
        msgs <- interpret env {envSystem = SystemPort (\_ _ -> throwIO (userError "no firefox")) (\_ -> pure ())} (OpenUrl XdgOpen "https://example.org/")
        msgs @?= [SystemFailed "user error (no firefox)"],
      testCase "CopyText copies the cleaned URL through sysCopy" $ do
        (env, calls) <- recordingEnv
        msgs <- interpret env (CopyText "https://m.imdb.com/title/tt0111161/")
        msgs @?= [Copied "https://imdb.com/title/tt0111161/"]
        readIORef calls >>= (@?= ["copy https://imdb.com/title/tt0111161/"])
    ]

-- | An Env whose desktop records its calls; Raindrop must not be reached.
recordingEnv :: IO (Env, IORef [String])
recordingEnv = do
  calls <- newIORef []
  events <- newBChan 1
  let record c = modifyIORef' calls (<> [c])
      system = SystemPort (\program (URL url) -> record ("open " <> program <> " " <> url)) (record . ("copy " <>))
  pure (Env events (RaindropPort (\_ -> fail "no Raindrop here")) system (pure (Now (read "2026-11-01 12:00:00 UTC") utc)), calls)

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
