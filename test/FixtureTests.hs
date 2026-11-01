{-# LANGUAGE OverloadedStrings #-}

module FixtureTests (fixtureTests) where

import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Fixture
import Network.Bookmark.Fake
import Options.Applicative (defaultPrefs, execParserPure, getParseResult, info, optional)
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
        parse ["--fixture", "seed.json"] @?= Just (Just (FixtureOpts "seed.json" [] Nothing 999)),
      testCase "repeated --fixture-fail, --fixture-dump and --fixture-archive" $
        parse ["--fixture", "seed.json", "--fixture-fail", "archive=503", "--fixture-fail", "set-reminders:2=404", "--fixture-dump", "out.json", "--fixture-archive", "7"]
          @?= Just (Just (FixtureOpts "seed.json" [FailRule StepArchive Nothing (FailHttp 503), FailRule StepSetReminders (Just 2) (FailHttp 404)] (Just "out.json") 7)),
      testCase "a bad failure rule is a usage error" $
        parse ["--fixture", "seed.json", "--fixture-fail", "archive=200"] @?= Nothing,
      testCase "fixture flags without --fixture are a usage error" $ do
        parse ["--fixture-fail", "archive=503"] @?= Nothing
        parse ["--fixture-dump", "out.json"] @?= Nothing,
      testCase "the store holds the seed's items, the rules and the archive id" $ do
        seed <- LBS.readFile "test/raindrop-items1.json"
        case fixtureStore (FixtureOpts "seed.json" [FailRule StepFetch Nothing FailConn] Nothing 7) seed of
          Left e -> assertFailure e
          Right fr -> (Map.size (fr ^. frItems), fr ^. frRules, fr ^. frArchive) @?= (18, [FailRule StepFetch Nothing FailConn], 7),
      testCase "a seed that is no list response is rejected" $
        either (const True) (const False) (fixtureStore (FixtureOpts "seed.json" [] Nothing 7) "{\"items\": 3}") @?= True
    ]
