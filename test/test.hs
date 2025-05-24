{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit

import           Control.Lens (view,_2)
import           Control.Lens.Operators
import           Data.List (find, head, last)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord

import           Network.Bookmark.Ui.State
import           Network.Bookmark.Types

import qualified Data.Aeson as A
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime(..), DiffTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Data.Time.Calendar (toGregorian)
import           Data.Ratio ((%))

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [hocketStateTests, raindropParsingTests, dateTimeParsingTests]

bookmarkItem1 :: BookmarkItem
bookmarkItem1 = BookmarkItem (BookmarkItemId "1")
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

bookmarkItem2 :: BookmarkItem
bookmarkItem2 = bookmarkItem1 {_biLastUpdate = read "2016-05-22 12:54:59 UTC", _biTitle = "newer title" }

testState = initialState (BookmarkCredentials (RaindropToken ""))

hocketStateTests = testGroup "HocketState"
  [ testCase "inserting items into the initial state" $
      length (insertItem bookmarkItem1 testState ^. hsContents) @?= 1
  , testCase "inserting an item that is present overwrites if newer" $
      let s = insertItems [bookmarkItem1,bookmarkItem2] testState
      in fmap (view (_2 . biTitle)) (Map.lookup (_biId bookmarkItem1) (view hsContents s)) @?= Just "newer title"
  , testCase "inserting an item that is present overwrites if newer, insertion order does not matter" $
      let s = insertItems [bookmarkItem2,bookmarkItem1] testState
      in fmap (view (_2 . biTitle)) (Map.lookup (_biId bookmarkItem1) (view hsContents s)) @?= Just "newer title"
  ]


raindropParsingTests :: TestTree
raindropParsingTests = testGroup "Raindrop JSON Parsing"
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
  ]

-- New test group for date/time parsing
dateTimeParsingTests :: TestTree
dateTimeParsingTests = testGroup "Date/Time Parsing" [testParseUTCTimeString]

testParseUTCTimeString :: TestTree
testParseUTCTimeString = testCase "parsing ISO8601 UTCTime string '2025-05-25T15:47:27.230Z'" $ do
  let timeString = "2025-05-25T15:47:27.230Z"
  -- Attempt to parse the string
  let mParsedTime = iso8601ParseM timeString :: Maybe UTCTime

  case mParsedTime of
    Nothing -> assertFailure $ "Failed to parse ZonedTime string: " ++ timeString
    Just (UTCTime day diffTime) -> do
      -- Check date components
      let (year, month, dayOfMonth) = toGregorian day
      assertEqual "Year component" 2025 year
      assertEqual "Month component" 5 month
      assertEqual "Day component" 25 dayOfMonth

      -- Check time component (DiffTime, seconds since midnight)
      -- 15h = 54000s; 47m = 2820s; Total = 54000 + 2820 + 27.230 = 56847.230s
      let expectedDiffTimeAsRational = 56847230 % 1000
      assertEqual "Time component (DiffTime as Rational)" expectedDiffTimeAsRational (toRational diffTime)

      -- UTCTime is implicitly UTC, so no TimeZone checks are needed.
