module IotaTime.Calendar.Iso

import public IotaTime.Calendar
import public IotaTime.Calendar.Gregorian
import Data.So

%default total

public export
gregorianDays : Year -> Integer -> Integer -> Integer
gregorianDays valueYear valueMonth valueDay =
  let yearNumber = yearValue valueYear
      shiftedYear = if valueMonth <= 2 then yearNumber - 1 else yearNumber
      era = shiftedYear `div` 400
      yearOfEra = shiftedYear - era * 400
      shiftedMonth = valueMonth + if valueMonth > 2 then -3 else 9
      dayOfYear = (153 * shiftedMonth + 2) `div` 5 + valueDay - 1
      dayOfEra = yearOfEra * 365 + yearOfEra `div` 4 -
        yearOfEra `div` 100 + dayOfYear
   in era * 146097 + dayOfEra - 730485

public export
isoWeekDateDays : WeekNumber -> DayOfWeek -> Year -> Integer
isoWeekDateDays week target valueYear =
  let januaryFourth = gregorianDays valueYear 1 4
      januaryFourthWeekday = (januaryFourth + 3) `mod` 7
      januaryFourthFromMonday =
        (januaryFourthWeekday - weekdayNumber Monday) `mod` 7
      targetFromMonday =
        (weekdayNumber target - weekdayNumber Monday) `mod` 7
   in januaryFourth - januaryFourthFromMonday +
      7 * (weekNumberValue week - 1) + targetFromMonday

public export
isValidIsoWeekDate : WeekNumber -> DayOfWeek -> Year -> Bool
isValidIsoWeekDate week target valueYear =
  isValidGregorianDays (isoWeekDateDays week target valueYear)

||| Construct a Gregorian date using ISO-8601 week numbering. Weeks start on
||| Monday and week 1 is the week containing January 4.
public export
fromWeekDate : (week : WeekNumber) -> (target : DayOfWeek) ->
               (valueYear : Year) ->
               {auto 0 valid : So (isValidIsoWeekDate week target valueYear)} ->
               CalendarDate Gregorian
fromWeekDate week target valueYear =
  gregorianFromDays (isoWeekDateDays week target valueYear) @{valid}

public export
data IsoWeekDateError = InvalidIsoWeekDate WeekNumber DayOfWeek Year

||| Validate an ISO week date learned at runtime.
public export
refineIsoWeekDate : WeekNumber -> DayOfWeek -> Year ->
                    Either IsoWeekDateError (CalendarDate Gregorian)
refineIsoWeekDate week target valueYear =
  case choose (isValidIsoWeekDate week target valueYear) of
    Left valid => Right (fromWeekDate week target valueYear @{valid})
    Right _ => Left (InvalidIsoWeekDate week target valueYear)
