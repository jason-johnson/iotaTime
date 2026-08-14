module Test.Iso

import IotaTime
import IotaTime.Calendar.Iso
import Test.Support

ymd : CalendarDate Gregorian -> (Year, Month, DayOfMonth)
ymd date = case yearMonthDay date of
  (valueYear ** (valueMonth, valueDay)) =>
    (valueYear, valueMonth, valueDay)

isoCivilDaysMatch : Bool
isoCivilDaysMatch =
  IotaTime.Calendar.Iso.daysFromCivil 2000 3 1 ==
    toDays (IotaTime.Calendar.Gregorian.calendarDate 1 March 2000) &&
  IotaTime.Calendar.Iso.daysFromCivil 2000 2 29 ==
    toDays (IotaTime.Calendar.Gregorian.calendarDate 29 February 2000) &&
  IotaTime.Calendar.Iso.daysFromCivil 1900 3 1 ==
    toDays (IotaTime.Calendar.Gregorian.calendarDate 1 March 1900)

isoCases : List RuntimeCase
isoCases =
  [ MkRuntimeCase "ISO civil days match Gregorian date coordinates"
      isoCivilDaysMatch
  , MkRuntimeCase "ISO week one can begin in the preceding year"
      (ymd (IotaTime.Calendar.Iso.fromWeekDate 1 Monday 2020) ==
        (2019, December, 30))
  , MkRuntimeCase "ISO week one Sunday follows the January 4 rule"
      (ymd (IotaTime.Calendar.Iso.fromWeekDate 1 Sunday 2000) ==
        (2000, January, 9))
  , MkRuntimeCase "ISO week five Sunday matches HodaTime"
      (ymd (IotaTime.Calendar.Iso.fromWeekDate 5 Sunday 2000) ==
        (2000, February, 6))
  , MkRuntimeCase "ISO years contain the correct 52 or 53 weeks"
      (weeksInIsoYear 2019 == 52 && weeksInIsoYear 2020 == 53 &&
       weeksInIsoYear 2021 == 52)
  , MkRuntimeCase "ISO week 53 is accepted only in a 53-week year"
      (ymd (IotaTime.Calendar.Iso.fromWeekDate 53 Sunday 2020) ==
         (2021, January, 3) &&
       case IotaTime.Calendar.Iso.refineWeekDate 53 Monday 2021 of
         Left (InvalidIsoWeekDate 53 Monday 2021) => True
         _ => False)
  , MkRuntimeCase "standards-valid ISO refinement rejects week zero"
      (case IotaTime.Calendar.Iso.refineWeekDate 0 Monday 2000 of
        Left (InvalidIsoWeekDate 0 Monday 2000) => True
        _ => False)
  , MkRuntimeCase "arithmetic ISO coordinates retain week zero"
      (case IotaTime.Calendar.Iso.refineArithmeticWeekDate 0 Monday 2000 of
        Right date => ymd date == (1999, December, 27)
        Left _ => False)
  , MkRuntimeCase "runtime ISO dates before the Gregorian boundary are rejected"
      (case IotaTime.Calendar.Iso.refineWeekDate 1 Monday 1582 of
        Left (InvalidIsoWeekDate 1 Monday 1582) => True
        _ => False)
  , MkRuntimeCase "runtime ISO dates at the Gregorian boundary are accepted"
      (case IotaTime.Calendar.Iso.refineWeekDate 41 Friday 1582 of
        Right date => ymd date == (1582, October, 15)
        Left _ => False)
  ]

export
run : IO Bool
run = runSuite "ISO week-date tests" isoCases
