{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Core.JsonTests
  ( raindropApiBodyTests,
    raindropParsingTests,
    dateTimeParsingTests,
    jsonRoundtripTests,
  )
where

import Control.Lens (view)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either (isLeft)
import Data.List (find)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Hocket.Adapter.Raindrop.Api (ApiResult (..), BatchMove (..), Created (..), Items (..), NewRaindrop (..), RaindropPatch (..))
import Hocket.Domain.Bookmark
import Test.Tasty
import Test.Tasty.HUnit
import TestUtil

-- Expected bodies are what the pre-servant wreq client sent.
raindropApiBodyTests :: TestTree
raindropApiBodyTests =
  testGroup
    "Raindrop API bodies"
    [ testCase "move to collection" $
        A.toJSON (MoveTo 7) @?= A.object ["collection" A..= A.object ["$id" A..= (7 :: Int)]],
      testCase "set reminder keeps millisecond Z format" $
        A.toJSON (SetReminderAt (UTCTime (read "2026-10-06") 18000))
          @?= A.object ["reminder" A..= A.object ["date" A..= ("2026-10-06T05:00:00.000Z" :: Text)]],
      testCase "clear reminder sends null" $
        A.toJSON ClearReminder @?= A.object ["reminder" A..= A.Null],
      testCase "batch move keeps ids as strings" $
        A.toJSON (BatchMove ["1", "2"] 7)
          @?= A.object ["ids" A..= (["1", "2"] :: [Text]), "collection" A..= A.object ["$id" A..= (7 :: Int)]],
      testCase "new raindrop" $
        A.toJSON (NewRaindrop "https://example.com" "-1" ["t"])
          @?= A.object ["link" A..= ("https://example.com" :: Text), "collection" A..= ("-1" :: Text), "tags" A..= (["t"] :: [Text]), "pleaseParse" A..= A.object []],
      testCase "created id is decoded" $
        (\(Created i) -> i) <$> A.decode "{\"result\":true,\"item\":{\"_id\":123}}" @?= Just 123,
      testCase "result false is a value, missing result is a decode failure" $ do
        (\(ApiResult b) -> b) <$> A.decode "{\"result\":false}" @?= Just False
        isLeft (A.eitherDecode @ApiResult "{}") @? "expected decode failure",
      testCase "highlight objects decode to their text" $
        view biHighlights <$> parseMaybe A.parseJSON (itemWithHighlights [A.object ["text" A..= ("quoted" :: Text), "color" A..= ("yellow" :: Text)]])
          @?= Just ["quoted"],
      testCase "list response decodes count and items" $ do
        jsonLBS <- LBS.readFile "test/raindrop-items1.json"
        case A.eitherDecode jsonLBS of
          Left e -> assertFailure e
          Right (Items count items) -> (count, length items) @?= (18, 18)
    ]

-- | The first item of the list fixture, with its highlights replaced.
itemWithHighlights :: [A.Value] -> A.Value
itemWithHighlights hs = case A.decode @A.Value (LBS.pack fixtureItem) of
  Just (A.Object o) -> A.Object (KM.insert "highlights" (A.toJSON hs) o)
  _ -> A.Null
  where
    fixtureItem = "{\"_id\":1,\"link\":\"https://example.com\",\"excerpt\":\"\",\"note\":\"\",\"type\":\"link\",\"tags\":[],\"removed\":false,\"created\":\"2026-01-01T00:00:00.000Z\",\"lastUpdate\":\"2026-01-01T00:00:00.000Z\",\"domain\":\"example.com\",\"title\":\"t\",\"sort\":0,\"collection\":{\"$id\":-1},\"important\":false}"

raindropParsingTests :: TestTree
raindropParsingTests =
  testGroup
    "Raindrop JSON Parsing"
    [ testCase "parsing list of BookmarkItems from JSON" $ do
        jsonLBS <- LBS.readFile "test/raindrop-items1.json"
        let decodedValue = A.eitherDecode jsonLBS :: Either String A.Value

        case decodedValue of
          Left err -> assertFailure $ "JSON decoding failed: " ++ err
          Right val ->
            case parseMaybe (A.withObject "ApiResponse" (A..: "items")) val of
              Nothing -> assertFailure "Could not extract 'items' array or parse it into [BookmarkItem]"
              Just (items :: [BookmarkItem]) -> do
                assertEqual "Number of parsed items" 18 (length items)
                assertEqual "First item ID" (BookmarkItemId "1085603154") (_biId $ head items)
                assertEqual "Last item ID" (BookmarkItemId "1070362199") (_biId $ last items)

                -- Verify a specific item's title (Google item)
                let googleItemId = BookmarkItemId "1077326683"
                let maybeGoogleItem = find (\item -> _biId item == googleItemId) items
                case maybeGoogleItem of
                  Nothing -> assertFailure $ "Item with ID " ++ show googleItemId ++ " not found."
                  Just googleItem -> assertEqual "Google item title" ("Google" :: Text) (_biTitle googleItem)

                -- Verify another specific item's excerpt (Stiftung Warentest item)
                let stiftungTestItemId = BookmarkItemId "1085443954"
                let maybeStiftungTestItem = find (\item -> _biId item == stiftungTestItemId) items
                case maybeStiftungTestItem of
                  Nothing -> assertFailure $ "Item with ID " ++ show stiftungTestItemId ++ " not found."
                  Just stiftungTestItem -> assertEqual "Stiftung Warentest item excerpt" ("Stiftung Warentest: Testberichte zu Elektronik, Haushalt und Gesundheit sowie Finanzen, Versicherung und Steuern" :: Text) (_biExcerpt stiftungTestItem)

                -- Verify specific item has reminder date
                let reminderItemId = BookmarkItemId "1076430230"
                let maybeReminderItem = find (\item -> _biId item == reminderItemId) items
                case maybeReminderItem of
                  Nothing -> assertFailure $ "Item with ID " ++ show reminderItemId ++ " not found."
                  Just reminderItem -> do
                    case _biReminder reminderItem of
                      Nothing -> assertFailure "Expected item to have a reminder, but found Nothing"
                      Just reminderTime -> assertEqual "Reminder date should match expected value" (read "2025-07-11 14:48:19.217 UTC") reminderTime
    ]

dateTimeParsingTests :: TestTree
dateTimeParsingTests = testGroup "Date/Time Parsing" [testParseUTCTimeString]

testParseUTCTimeString :: TestTree
testParseUTCTimeString = testCase "parsing ISO8601 UTCTime string '2025-05-25T15:47:27.230Z'" $ do
  let timeString = "2025-05-25T15:47:27.230Z"
  let mParsedTime = iso8601ParseM timeString :: Maybe UTCTime

  case mParsedTime of
    Nothing -> assertFailure $ "Failed to parse ZonedTime string: " ++ timeString
    Just (UTCTime day diffTime) -> do
      let (year, month, dayOfMonth) = toGregorian day
      assertEqual "Year component" 2025 year
      assertEqual "Month component" 5 month
      assertEqual "Day component" 25 dayOfMonth

      let expectedDiffTimeAsRational = 56847230 % 1000
      assertEqual "Time component (DiffTime as Rational)" expectedDiffTimeAsRational (toRational diffTime)

jsonRoundtripTests :: TestTree
jsonRoundtripTests =
  testGroup
    "JSON Roundtrip"
    [ testBookmarkItemRoundtrip,
      testBookmarkItemEdgeCases
    ]

testBookmarkItemRoundtrip :: TestTree
testBookmarkItemRoundtrip = testCase "BookmarkItem JSON encode/decode roundtrip" $ do
  let original = bookmarkItem1
  let encoded = A.encode original
  let decoded = A.eitherDecode encoded :: Either String BookmarkItem

  case decoded of
    Left err -> assertFailure $ "JSON roundtrip failed: " ++ err
    Right item -> assertEqual "Roundtrip should preserve original item" original item

testBookmarkItemEdgeCases :: TestTree
testBookmarkItemEdgeCases = testCase "BookmarkItem edge cases" $ do
  let itemWithEmptyFields =
        BookmarkItem
          (BookmarkItemId "999")
          "" -- empty link
          "" -- empty excerpt
          "" -- empty note
          "link"
          [] -- empty tags
          True
          (read "2016-05-22 12:33:11 UTC")
          (read "2016-05-22 12:54:59 UTC")
          "" -- empty domain
          "" -- empty title
          0
          [] -- empty highlights
          0
          False
          Nothing -- no reminder
  let encoded = A.encode itemWithEmptyFields
  let decoded = A.eitherDecode encoded :: Either String BookmarkItem

  case decoded of
    Left err -> assertFailure $ "Edge case roundtrip failed: " ++ err
    Right item -> assertEqual "Empty fields should roundtrip correctly" itemWithEmptyFields item
