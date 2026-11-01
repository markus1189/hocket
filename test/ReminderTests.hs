module ReminderTests (reminderTests) where

import Data.Time (UTCTime, addDays)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), TimeZone, hoursToTimeZone, localTimeToUTC, utcToLocalTime)
import Gen (genTimeZone, genUTCTime)
import Hedgehog (assert, forAll, property, (===))
import Network.Bookmark.Reminder (reminderPolicy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

cet, cest :: TimeZone
cet = hoursToTimeZone 1
cest = hoursToTimeZone 2

utc :: String -> UTCTime
utc = read

reminderTests :: TestTree
reminderTests =
  testGroup
    "reminderPolicy"
    [ testCase "23:59 local → 07:00 the next local day" $
        reminderPolicy cet (utc "2026-03-10 22:59:00 UTC") @?= utc "2026-03-11 06:00:00 UTC",
      testCase "00:00 local belongs to the new day" $
        reminderPolicy cet (utc "2026-03-10 23:00:00 UTC") @?= utc "2026-03-12 06:00:00 UTC",
      testCase "evening before the Europe/Berlin change, CEST offset" $
        -- fixed-offset TimeZone: 06:00 CET wall clock after the change; see plan §11 #15
        reminderPolicy cest (utc "2026-10-24 20:00:00 UTC") @?= utc "2026-10-25 05:00:00 UTC",
      testCase "evening before the spring change, CET offset" $
        -- fixed-offset TimeZone: 08:00 CEST wall clock after the change; see plan §11 #15
        reminderPolicy cet (utc "2026-03-28 21:00:00 UTC") @?= utc "2026-03-29 06:00:00 UTC",
      testCase "evening of the change, CET offset" $
        reminderPolicy cet (utc "2026-10-25 21:00:00 UTC") @?= utc "2026-10-26 06:00:00 UTC",
      testProperty "07:00 local on the next local day, always in the future" $ property $ do
        tz <- forAll genTimeZone
        now <- forAll genUTCTime
        let result = reminderPolicy tz now
            tomorrow = addDays 1 (localDay (utcToLocalTime tz now))
        utcToLocalTime tz result === LocalTime tomorrow (TimeOfDay 7 0 0)
        assert (result > now)
    ]
