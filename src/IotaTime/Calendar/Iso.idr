module IotaTime.Calendar.Iso

import public IotaTime.Calendar
import public IotaTime.Calendar.Gregorian
import Data.So

%default total

public export
daysFromCivil : Year -> Integer -> Integer -> Integer
daysFromCivil valueYear valueMonth valueDay =
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
weekDateDays : WeekNumber -> DayOfWeek -> Year -> Integer
weekDateDays week target valueYear =
  let januaryFourth = daysFromCivil valueYear 1 4
      januaryFourthWeekday = (januaryFourth + 3) `mod` 7
      januaryFourthFromMonday =
        (januaryFourthWeekday - weekdayNumber Monday) `mod` 7
      targetFromMonday =
        (weekdayNumber target - weekdayNumber Monday) `mod` 7
   in januaryFourth - januaryFourthFromMonday +
      7 * (weekNumberValue week - 1) + targetFromMonday

||| Whether an ISO week-numbering year contains week 53.
public export
hasFiftyThreeWeeks : Year -> Bool
hasFiftyThreeWeeks valueYear =
  let januaryFirstWeekday = (daysFromCivil valueYear 1 1 + 3) `mod` 7
   in januaryFirstWeekday == weekdayNumber Thursday ||
      (januaryFirstWeekday == weekdayNumber Wednesday &&
       IotaTime.Calendar.Gregorian.isLeapYear valueYear)

||| The number of ISO weeks in a year, either 52 or 53.
public export
weeksInIsoYear : Year -> WeekNumber
weeksInIsoYear valueYear =
  if hasFiftyThreeWeeks valueYear then 53 else 52

||| Whether a week number belongs to the requested ISO week-numbering year.
public export
isValidIsoWeekNumber : WeekNumber -> Year -> Bool
isValidIsoWeekNumber week valueYear =
  let value = weekNumberValue week
   in value >= 1 &&
      (value <= 52 || (value == 53 && hasFiftyThreeWeeks valueYear))

||| Whether an unrestricted arithmetic week coordinate resolves inside the
||| supported Gregorian range.
public export
isValidArithmeticWeekDate : WeekNumber -> DayOfWeek -> Year -> Bool
isValidArithmeticWeekDate week target valueYear =
  IotaTime.Calendar.isValidDays {calendar = Gregorian}
    (weekDateDays week target valueYear)

||| Whether a standards-valid ISO week date resolves inside the supported
||| Gregorian range.
public export
isValidWeekDate : WeekNumber -> DayOfWeek -> Year -> Bool
isValidWeekDate week target valueYear =
  isValidIsoWeekNumber week valueYear &&
  isValidArithmeticWeekDate week target valueYear

0 andRight : (left, right : Bool) -> So (left && right) -> So right
andRight True True Oh = Oh

||| Construct an unrestricted arithmetic ISO week coordinate. Unlike
||| `fromWeekDate`, this operation deliberately permits week zero and values
||| beyond the requested ISO year.
public export
arithmeticFromWeekDate : (week : WeekNumber) -> (target : DayOfWeek) ->
                         (valueYear : Year) ->
                         {auto 0 valid : So
                           (isValidArithmeticWeekDate
                             week target valueYear)} ->
                         CalendarDate Gregorian
arithmeticFromWeekDate week target valueYear =
  IotaTime.Calendar.Gregorian.fromDays
    (weekDateDays week target valueYear) @{valid}

||| Construct a Gregorian date using ISO-8601 week numbering. Weeks start on
||| Monday, week 1 contains January 4, and the week must belong to the requested
||| ISO week-numbering year.
public export
fromWeekDate : (week : WeekNumber) -> (target : DayOfWeek) ->
               (valueYear : Year) ->
               {auto 0 valid : So
                 (IotaTime.Calendar.Iso.isValidWeekDate
                   week target valueYear)} ->
               CalendarDate Gregorian
fromWeekDate week target valueYear @{valid} =
  let 0 arithmeticValid : So
        (isValidArithmeticWeekDate week target valueYear)
      arithmeticValid = andRight
        (isValidIsoWeekNumber week valueYear)
        (isValidArithmeticWeekDate week target valueYear)
        valid
   in arithmeticFromWeekDate week target valueYear @{arithmeticValid}

public export
data IsoWeekDateError
  = InvalidIsoWeekDate WeekNumber DayOfWeek Year
  | ArithmeticIsoWeekDateOutOfRange WeekNumber DayOfWeek Year

||| Validate an ISO week date learned at runtime.
public export
refineWeekDate : WeekNumber -> DayOfWeek -> Year ->
                 Either IsoWeekDateError (CalendarDate Gregorian)
refineWeekDate week target valueYear =
  case choose
    (IotaTime.Calendar.Iso.isValidWeekDate week target valueYear) of
    Left valid => Right
      (IotaTime.Calendar.Iso.fromWeekDate week target valueYear @{valid})
    Right _ => Left (InvalidIsoWeekDate week target valueYear)

||| Validate an unrestricted arithmetic week coordinate learned at runtime.
public export
refineArithmeticWeekDate : WeekNumber -> DayOfWeek -> Year ->
  Either IsoWeekDateError (CalendarDate Gregorian)
refineArithmeticWeekDate week target valueYear =
  case choose (isValidArithmeticWeekDate week target valueYear) of
    Left valid => Right
      (arithmeticFromWeekDate week target valueYear @{valid})
    Right _ => Left
      (ArithmeticIsoWeekDateOutOfRange week target valueYear)
