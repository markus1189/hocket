module Hocket.Domain.Reminder (reminderPolicy) where

import Data.Time (UTCTime, addDays)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), TimeZone, localTimeToUTC, utcToLocalTime)

-- | 07:00 on the next local day. 'TimeZone' is a fixed offset, so across a
-- DST change the result keeps today's offset (plan §11 #15).
reminderPolicy :: TimeZone -> UTCTime -> UTCTime
reminderPolicy tz now =
  localTimeToUTC tz (LocalTime (addDays 1 (localDay (utcToLocalTime tz now))) (TimeOfDay 7 0 0))
