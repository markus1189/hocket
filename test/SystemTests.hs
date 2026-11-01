{-# LANGUAGE OverloadedStrings #-}

module SystemTests (systemTests) where

import CharacterizationTests (emptyState, item)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (fromException, throwIO)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (forever)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Time (utc)
import Hocket.Adapter.System (cleanUrl, urlReplacements)
import Hocket.App.Effect (Effect (..), Opener (..))
import Hocket.App.Model (insertItems, mStatus)
import Hocket.App.Msg (Intent (..), Msg (..), Now (..), Target (..))
import Hocket.Domain.Bookmark (BookmarkItemId (..), URL (..), biLink)
import Hocket.Port (RaindropPort (..), SystemPort (..))
import Hocket.Shell.Interpret (Env (..), copyCleaned, interpret, newEnv, step)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

systemTests :: TestTree
systemTests = testGroup "system" [cleanUrlTests, copyTests, systemPortTests, stepTests]

-- | The step both event loops run.
stepTests :: TestTree
stepTests =
  testGroup
    "step"
    [ testCase "an effect's immediate answer goes through update in the same step" $ do
        (env, calls) <- recordingEnv
        let s = insertItems [item "a" (-1) & biLink .~ "https://m.imdb.com/a"] emptyState
        (s', halted) <- step env (Intent (CopyUrl (ById (BookmarkItemId "a")))) s
        (halted, s' ^. mStatus) @?= (False, Just "Copied: https://imdb.com/a")
        readIORef calls >>= (@?= ["copy https://imdb.com/a"]),
      testCase "Halt is reported, not acted on" $ do
        (env, _) <- recordingEnv
        (_, halted) <- step env (Intent Quit) emptyState
        halted @?= True
    ]

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
      testCase "a cancel during the browser or the copy is rethrown, not reported" $
        for_ [OpenUrl Firefox "https://example.org/", CopyText "https://example.org/"] $ \effect -> do
          entered <- newEmptyMVar
          (env, _) <- recordingEnv
          let hang = putMVar entered () >> forever (threadDelay 1000000)
          r <- withAsync (interpret env {envSystem = SystemPort (\_ _ -> hang) (const hang)} effect) $ \running -> do
            takeMVar entered
            cancel running
            waitCatch running
          case r of
            Left e -> fromException e @?= Just AsyncCancelled
            Right msgs -> assertFailure ("interpret answered " <> show msgs <> " to a cancel"),
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
  let record c = modifyIORef' calls (<> [c])
      system = SystemPort (\program (URL url) -> record ("open " <> program <> " " <> url)) (record . ("copy " <>))
  env <- newEnv (RaindropPort (\_ -> fail "no Raindrop here")) system (pure (Now (read "2026-11-01 12:00:00 UTC") utc))
  pure (env, calls)

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
