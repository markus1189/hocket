{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens (ix, view, _1, _2)
import Control.Lens.Operators
import qualified Data.Aeson as A
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (find, head, last)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (DiffTime, UTCTime (..))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [hocketStateTests, raindropParsingTests, dateTimeParsingTests, jsonRoundtripTests]

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

bookmarkItem2 :: BookmarkItem
bookmarkItem2 = bookmarkItem1 {_biLastUpdate = read "2016-05-22 12:54:59 UTC", _biTitle = "newer title"}

bookmarkItem1_same_ts_diff_title :: BookmarkItem
bookmarkItem1_same_ts_diff_title = bookmarkItem1 {_biTitle = "title for same ts item"}

testState = initialState (BookmarkCredentials (RaindropToken "") 0)

hocketStateTests =
  testGroup
    "HocketState insertItem/insertItems"
    [ testCase "insertItem: new item gets None status and correct data" $
        let s = insertItem bookmarkItem1 testState
         in Map.lookup (_biId bookmarkItem1) (view hsContents s) @?= Just (None, bookmarkItem1),
      testCase "insertItem: updates with newer item" $
        let stateWithOldItem = insertItem bookmarkItem1 testState
            stateWithOldItemPending = stateWithOldItem & hsContents . ix (_biId bookmarkItem1) . _1 .~ ToBeArchived
            stateAfterUpdate = insertItem bookmarkItem2 stateWithOldItemPending
         in Map.lookup (_biId bookmarkItem1) (view hsContents stateAfterUpdate) @?= Just (ToBeArchived, bookmarkItem2),
      testCase "insertItem: older item does not overwrite newer; status and data preserved" $
        let stateWithNewerItem = insertItem bookmarkItem2 testState
            stateWithNewerItemPending = stateWithNewerItem & hsContents . ix (_biId bookmarkItem2) . _1 .~ ToBeArchived
            stateAfterAttemptedUpdate = insertItem bookmarkItem1 stateWithNewerItemPending
         in Map.lookup (_biId bookmarkItem2) (view hsContents stateAfterAttemptedUpdate) @?= Just (ToBeArchived, bookmarkItem2),
      testCase "insertItem: item with same timestamp does overwrite; status preserved, data updated" $
        let stateWithOriginalItem = insertItem bookmarkItem1 testState
            stateWithOriginalItemPending = stateWithOriginalItem & hsContents . ix (_biId bookmarkItem1) . _1 .~ ToBeArchived
            stateAfterAttemptedUpdate = insertItem bookmarkItem1_same_ts_diff_title stateWithOriginalItemPending
         in Map.lookup (_biId bookmarkItem1) (view hsContents stateAfterAttemptedUpdate) @?= Just (ToBeArchived, bookmarkItem1_same_ts_diff_title),
      testCase "insertItems: inserting an item that is present overwrites if newer" $
        let s = insertItems [bookmarkItem1, bookmarkItem2] testState
         in fmap (view (_2 . biTitle)) (Map.lookup (_biId bookmarkItem1) (view hsContents s)) @?= Just "newer title",
      testCase "insertItems: inserting an item that is present overwrites if newer, insertion order does not matter" $
        let s = insertItems [bookmarkItem2, bookmarkItem1] testState
         in fmap (view (_2 . biTitle)) (Map.lookup (_biId bookmarkItem1) (view hsContents s)) @?= Just "newer title",
      testCase "insertItems: basic insertion count" $
        length (insertItems [bookmarkItem1] testState ^. hsContents) @?= 1
    ]

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
