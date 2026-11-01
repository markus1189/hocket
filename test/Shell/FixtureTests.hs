{-# LANGUAGE OverloadedStrings #-}

module Shell.FixtureTests (fixtureTests) where

import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Hocket.Adapter.Fake
import Hocket.Domain.Bookmark (URL (..))
import Hocket.Port (SystemPort (..))
import Hocket.Shell.Fixture
import Options.Applicative (defaultPrefs, execParserPure, getParseResult, info, optional)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

-- | The flags as `hocket tui` parses them: absent, or the whole group.
parse :: [String] -> Maybe (Maybe FixtureOpts)
parse = getParseResult . execParserPure defaultPrefs (info (optional fixtureParser) mempty)

fixtureTests :: TestTree
fixtureTests =
  testGroup
    "fixture mode"
    [ testCase "no fixture flags: the real account" $
        parse [] @?= Just Nothing,
      testCase "--fixture alone: no failures, no dump, archive 999" $
        parse ["--fixture", "seed.json"] @?= Just (Just (FixtureOpts "seed.json" [] Nothing 999 Nothing)),
      testCase "repeated --fixture-fail, --fixture-dump and --fixture-archive" $
        parse ["--fixture", "seed.json", "--fixture-fail", "archive=503", "--fixture-fail", "set-reminders:2=404", "--fixture-dump", "out.json", "--fixture-archive", "7"]
          @?= Just (Just (FixtureOpts "seed.json" [FailRule StepArchive Nothing (FailHttp 503), FailRule StepSetReminders (Just 2) (FailHttp 404)] (Just "out.json") 7 Nothing)),
      testCase "a bad failure rule is a usage error" $
        parse ["--fixture", "seed.json", "--fixture-fail", "archive=200"] @?= Nothing,
      testCase "fixture flags without --fixture are a usage error" $ do
        parse ["--fixture-fail", "archive=503"] @?= Nothing
        parse ["--fixture-dump", "out.json"] @?= Nothing
        parse ["--fixture-desktop-log", "out.jsonl"] @?= Nothing,
      testCase "--fixture-desktop-log parses with --fixture" $
        fmap (fmap foDesktopLog) (parse ["--fixture", "seed.json", "--fixture-desktop-log", "d.jsonl"]) @?= Just (Just (Just "d.jsonl")),
      testCase "the desktop log is one JSON line per open or copy, and nothing starts" $
        withSystemTempDirectory "hk" $ \dir -> do
          let out = dir </> "desktop.jsonl"
              port = recordingSystemPort (Just out)
          sysOpenUrl port "firefox" (URL "https://example.org/a?b=\"c\"")
          sysOpenUrl port "xdg-open" (URL "https://app.raindrop.io/my/-1/item/7/edit")
          sysCopy port "https://example.org/x"
          LBS.readFile out
            >>= ( @?=
                    "{\"open\":{\"program\":\"firefox\",\"url\":\"https://example.org/a?b=\\\"c\\\"\"}}\n\
                    \{\"open\":{\"program\":\"xdg-open\",\"url\":\"https://app.raindrop.io/my/-1/item/7/edit\"}}\n\
                    \{\"copy\":\"https://example.org/x\"}\n"
                ),
      testCase "without a desktop log nothing is written" $
        withSystemTempDirectory "hk" $ \dir -> do
          let port = recordingSystemPort Nothing
          sysOpenUrl port "firefox" (URL "https://example.org/a")
          sysCopy port "x"
          listDirectory dir >>= (@?= []),
      testCase "the store holds the seed's items, the rules and the archive id" $ do
        seed <- LBS.readFile "test/raindrop-items1.json"
        case fixtureStore (FixtureOpts "seed.json" [FailRule StepFetch Nothing FailConn] Nothing 7 Nothing) seed of
          Left e -> assertFailure e
          Right fr -> (Map.size (fr ^. frItems), fr ^. frRules, fr ^. frArchive) @?= (18, [FailRule StepFetch Nothing FailConn], 7),
      testCase "a seed that is no list response is rejected" $
        either (const True) (const False) (fixtureStore (FixtureOpts "seed.json" [] Nothing 7 Nothing) "{\"items\": 3}") @?= True
    ]
