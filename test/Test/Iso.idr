module Test.Iso

import IotaTime
import IotaTime.Calendar.Iso
import Test.Support

ymd : CalendarDate Gregorian -> (Year, Month, DayOfMonth)
ymd date = case yearMonthDay {calendar = Gregorian} date of
  (valueYear ** (valueMonth, valueDay)) =>
    (valueYear, valueMonth, valueDay)

isoCases : List RuntimeCase
isoCases =
  [ MkRuntimeCase "ISO week one can begin in the preceding year"
      (ymd (IotaTime.Calendar.Iso.fromWeekDate 1 Monday 2020) ==
        (2019, December, 30))
  , MkRuntimeCase "ISO week one Sunday follows the January 4 rule"
      (ymd (IotaTime.Calendar.Iso.fromWeekDate 1 Sunday 2000) ==
        (2000, January, 9))
  , MkRuntimeCase "ISO week five Sunday matches HodaTime"
      (ymd (IotaTime.Calendar.Iso.fromWeekDate 5 Sunday 2000) ==
        (2000, February, 6))
  , MkRuntimeCase "ISO week zero remains arithmetic"
      (case refineIsoWeekDate 0 Monday 2000 of
        Right date => ymd date == (1999, December, 27)
        Left _ => False)
  , MkRuntimeCase "runtime ISO dates before the Gregorian boundary are rejected"
      (case refineIsoWeekDate 1 Monday 1582 of
        Left (InvalidIsoWeekDate 1 Monday 1582) => True
        _ => False)
  , MkRuntimeCase "runtime ISO dates at the Gregorian boundary are accepted"
      (case refineIsoWeekDate 41 Friday 1582 of
        Right date => ymd date == (1582, October, 15)
        Left _ => False)
  ]

export
run : IO Bool
run = runSuite "ISO week-date tests" isoCases
