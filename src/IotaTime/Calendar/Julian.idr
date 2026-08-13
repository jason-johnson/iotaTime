module IotaTime.Calendar.Julian

import IotaTime.Internal.ApplyPeriod
import IotaTime.Calendar
import IotaTime.Period
import Data.So
import Derive.Prelude

%language ElabReflection

%default total

||| The proleptic Julian calendar, supported from January 1, 45 BC.
public export
data Julian = JulianCalendar

namespace JulianMonths
  public export
  data JulianMonth
    = January | February | March | April | May | June
    | July | August | September | October | November | December

  public export
  monthNumber : JulianMonth -> Integer
  monthNumber January = 1
  monthNumber February = 2
  monthNumber March = 3
  monthNumber April = 4
  monthNumber May = 5
  monthNumber June = 6
  monthNumber July = 7
  monthNumber August = 8
  monthNumber September = 9
  monthNumber October = 10
  monthNumber November = 11
  monthNumber December = 12

  public export
  Eq JulianMonth where
    left == right = monthNumber left == monthNumber right

  public export
  Ord JulianMonth where
    compare left right = compare (monthNumber left) (monthNumber right)

  %runElab derive `{JulianMonth} [Show]

monthFromNumber : Integer -> JulianMonth
monthFromNumber 1 = JulianMonths.January
monthFromNumber 2 = JulianMonths.February
monthFromNumber 3 = JulianMonths.March
monthFromNumber 4 = JulianMonths.April
monthFromNumber 5 = JulianMonths.May
monthFromNumber 6 = JulianMonths.June
monthFromNumber 7 = JulianMonths.July
monthFromNumber 8 = JulianMonths.August
monthFromNumber 9 = JulianMonths.September
monthFromNumber 10 = JulianMonths.October
monthFromNumber 11 = JulianMonths.November
monthFromNumber _ = JulianMonths.December

export
record JulianDate where
  constructor MkJulianDate
  daysSinceEpoch : Integer
  0 validDays : So (daysSinceEpoch >= -746631)

public export
Eq JulianDate where
  left == right = left.daysSinceEpoch == right.daysSinceEpoch

public export
Ord JulianDate where
  compare left right = compare left.daysSinceEpoch right.daysSinceEpoch

||| Whether a Julian year is divisible by four and therefore leap.
public export
isLeapYear : Year -> Bool
isLeapYear value = yearValue value `mod` 4 == 0

public export
maxDaysInMonth : JulianMonth -> Year -> DayOfMonth
maxDaysInMonth JulianMonths.February value = if isLeapYear value then 29 else 28
maxDaysInMonth JulianMonths.April _ = 30
maxDaysInMonth JulianMonths.June _ = 30
maxDaysInMonth JulianMonths.September _ = 30
maxDaysInMonth JulianMonths.November _ = 30
maxDaysInMonth _ _ = 31

daysFromJulianCivil : Year -> JulianMonth -> DayOfMonth -> Integer
daysFromJulianCivil valueYear valueMonth valueDay =
  let number = JulianMonths.monthNumber valueMonth
      shiftedYear = yearValue valueYear - if number <= 2 then 1 else 0
      relativeYear = shiftedYear - 2000
      shiftedMonth = number + if number > 2 then -3 else 9
      dayOfYear = (153 * shiftedMonth + 2) `div` 5 + dayOfMonthValue valueDay - 1
   in relativeYear * 365 + relativeYear `div` 4 + dayOfYear

julianCivilFromDays : Integer -> (Year, JulianMonth, DayOfMonth)
julianCivilFromDays value =
  let era : Integer
      era = value `div` 1461
      dayOfEra = value - era * 1461
      yearOfEra : Integer
      yearOfEra = min 3 (dayOfEra `div` 365)
      dayOfYear = dayOfEra - yearOfEra * 365
      shiftedMonth = (5 * dayOfYear + 2) `div` 153
      dayNumber = dayOfYear - (153 * shiftedMonth + 2) `div` 5 + 1
      monthNumber = shiftedMonth + if shiftedMonth < 10 then 3 else -9
      yearNumber = 2000 + era * 4 + yearOfEra + if monthNumber <= 2 then 1 else 0
   in (yearFromInteger yearNumber, monthFromNumber monthNumber,
       dayOfMonthFromInteger dayNumber)

||| The Julian calendar epoch day, representing January 1, 45 BC.
public export
epochDay : Integer
epochDay = -746631

checkedJulianDate : (days : Integer) ->
                    (0 valid : So (days >= -746631)) -> JulianDate
checkedJulianDate days valid = MkJulianDate days valid

export
HasCalendarBridge JulianDate where
  toBridgeDays date = date.daysSinceEpoch + 13
  acceptsBridgeDays days = days - 13 >= epochDay
  fromBridgeDays days @{valid} = checkedJulianDate (days - 13) valid
  bridgeCalendarName = "Julian"

public export
isValidDate : DayOfMonth -> JulianMonth -> Year -> Bool
isValidDate valueDay valueMonth valueYear =
  dayOfMonthValue valueDay <= dayOfMonthValue (maxDaysInMonth valueMonth valueYear) &&
  yearValue valueYear >= -44

clampToJulian : Integer -> Integer
clampToJulian = max epochDay

makeJulianDate : Integer -> JulianDate
makeJulianDate days =
  let clamped = clampToJulian days
  in case choose (clamped >= -746631) of
        Left valid => checkedJulianDate clamped valid
        Right _ => checkedJulianDate epochDay Oh

shiftJulianDays : Integer -> JulianDate -> JulianDate
shiftJulianDays amount date = makeJulianDate (date.daysSinceEpoch + amount)

shiftJulianMonths : Integer -> JulianDate -> JulianDate
shiftJulianMonths amount date =
  let (valueYear, valueMonth, valueDay) = julianCivilFromDays date.daysSinceEpoch
      monthOrdinal = JulianMonths.monthNumber valueMonth - 1 + amount
      targetYear = yearFromInteger (yearValue valueYear + monthOrdinal `div` 12)
      targetMonth = monthFromNumber (monthOrdinal `mod` 12 + 1)
      targetDay = min valueDay (maxDaysInMonth targetMonth targetYear)
  in makeJulianDate (daysFromJulianCivil targetYear targetMonth targetDay)

shiftJulianYears : Integer -> JulianDate -> JulianDate
shiftJulianYears amount date =
  let (valueYear, valueMonth, valueDay) = julianCivilFromDays date.daysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + amount)
      targetDay = min valueDay (maxDaysInMonth valueMonth targetYear)
  in makeJulianDate (daysFromJulianCivil targetYear valueMonth targetDay)

applyJulianPeriod : Period target -> JulianDate -> JulianDate
applyJulianPeriod = applyDatePeriodWith
  shiftJulianYears shiftJulianMonths shiftJulianDays

julianDayOfWeek : JulianDate -> DayOfWeek
julianDayOfWeek date = weekdayFromNumber (date.daysSinceEpoch + 2)

nextJulian : Integer -> DayOfWeek -> JulianDate -> JulianDate
nextJulian count target date =
  makeJulianDate (date.daysSinceEpoch +
    nextWeekdayOffset count (julianDayOfWeek date) target)

previousJulian : Integer -> DayOfWeek -> JulianDate -> JulianDate
previousJulian count target date =
  makeJulianDate (date.daysSinceEpoch +
    previousWeekdayOffset count (julianDayOfWeek date) target)

public export
Calendar Julian where
  DateRep = JulianDate
  MonthRep _ = JulianMonth

  isValidDays = (>= epochDay)
  fromDays days @{valid} = checkedJulianDate days valid
  toDaysFor date = date.daysSinceEpoch
  toDaysValid (MkJulianDate _ valid) = valid
  toFromDays _ _ = Refl
  fromToDays (MkJulianDate _ _) = Refl
  calendarName = "Julian"

  year' date = let (value, _, _) = julianCivilFromDays date.daysSinceEpoch in value
  toYmd date = let (_, valueMonth, valueDay) = julianCivilFromDays date.daysSinceEpoch
                in (valueMonth, valueDay)
  day' date = let (_, _, value) = julianCivilFromDays date.daysSinceEpoch in value
  month' date = let (_, value, _) = julianCivilFromDays date.daysSinceEpoch in value

  applyCalendarPeriod' = applyJulianPeriod
  shiftCalendarDays' = shiftJulianDays

  dayOfWeekFor = julianDayOfWeek
  nextFor = nextJulian
  previousFor = previousJulian

public export
Show JulianDate where
  show date = case julianCivilFromDays date.daysSinceEpoch of
    (valueYear, valueMonth, valueDay) =>
      "calendarDate " ++ show valueDay ++ " " ++
      show valueMonth ++ " " ++ show valueYear

public export
HasCalendar JulianDate where
  calendarCapability = ()

public export
PeriodTarget JulianDate where
  periodTarget = ()

public export
ApplyPeriod JulianDate where
  applyPeriod = applyJulianPeriod

public export
CalendarValue JulianDate where
  CalendarMonth _ = JulianMonth
  calendarValueToDays = toDaysFor {calendar = Julian}
  calendarValueYear = yearFor {calendar = Julian}
  calendarValueMonthDay = toYmd {calendar = Julian}
  calendarValueDayOfWeek = dayOfWeekFor {calendar = Julian}
  calendarValueBetweenWith = betweenWithFor {calendar = Julian}

public export
CalendarNavigation JulianDate where
  calendarValueNext = nextFor {calendar = Julian}
  calendarValuePrevious = previousFor {calendar = Julian}

||| Construct a statically validated Julian date.
public export
calendarDate : (valueDay : DayOfMonth) -> (valueMonth : JulianMonth) ->
             (valueYear : Year) ->
             {auto 0 valid : So (isValidDate valueDay valueMonth valueYear)} ->
             CalendarDate Julian
calendarDate valueDay valueMonth valueYear =
  makeJulianDate (daysFromJulianCivil valueYear valueMonth valueDay)

||| Failures produced while refining untrusted Julian date data.
public export
data JulianDateError
  = InvalidJulianDate DayOfMonth JulianMonth Year
  | InvalidJulianDayCount Integer
  | InvalidJulianNthDay DayNth DayOfWeek JulianMonth Year
  | InvalidJulianWeekDate WeekNumber DayOfWeek Year

||| Validate runtime day, month, and year components as a Julian date.
public export
refineDate : DayOfMonth -> JulianMonth -> Year ->
                   Either JulianDateError (CalendarDate Julian)
refineDate valueDay valueMonth valueYear =
  case choose (isValidDate valueDay valueMonth valueYear) of
    Left valid => Right (calendarDate valueDay valueMonth valueYear @{valid})
    Right _ => Left (InvalidJulianDate valueDay valueMonth valueYear)

||| Construct a Julian date from a statically valid calendar-relative day count.
public export
fromDays : (days : Integer) -> {auto 0 valid : So
  (IotaTime.Calendar.isValidDays {calendar = Julian} days)} ->
                 CalendarDate Julian
fromDays days @{valid} = checkedJulianDate days valid

||| Validate a runtime Julian day count.
public export
refineDays : Integer -> Either JulianDateError (CalendarDate Julian)
refineDays days = case choose
  (IotaTime.Calendar.isValidDays {calendar = Julian} days) of
  Left valid => Right (fromDays days @{valid})
  Right _ => Left (InvalidJulianDayCount days)

nthJulianDayOfMonth : DayNth -> DayOfWeek -> JulianMonth -> Year -> DayOfMonth
nthJulianDayOfMonth nth target valueMonth valueYear =
  let monthLength = maxDaysInMonth valueMonth valueYear
      firstDate = makeJulianDate (daysFromJulianCivil valueYear valueMonth 1)
      firstOffset =
        (weekdayNumber target - weekdayNumber (julianDayOfWeek firstDate))
           `mod` daysPerWeek
      lastDate = makeJulianDate (daysFromJulianCivil valueYear valueMonth monthLength)
      lastOffset =
        (weekdayNumber (julianDayOfWeek lastDate) - weekdayNumber target)
          `mod` daysPerWeek
      dayNumber = nthWeekdayDayNumber nth (dayOfMonthValue monthLength)
        firstOffset lastOffset
   in dayOfMonthFromInteger dayNumber

public export
isValidNthDay : DayNth -> DayOfWeek -> JulianMonth -> Year -> Bool
isValidNthDay nth target valueMonth valueYear =
  if yearValue valueYear > -44
    then case nth of
      Fifth => nthJulianDayOfMonth nth target valueMonth valueYear <=
        maxDaysInMonth valueMonth valueYear
      _ => True
    else isValidDate
      (nthJulianDayOfMonth nth target valueMonth valueYear) valueMonth valueYear

||| Construct the nth requested weekday in a Julian month.
public export
fromNthDay : (nth : DayNth) -> (target : DayOfWeek) ->
                   (valueMonth : JulianMonth) -> (valueYear : Year) ->
                   {auto 0 valid : So
                     (isValidNthDay nth target valueMonth valueYear)} ->
                   CalendarDate Julian
fromNthDay nth target valueMonth valueYear =
  makeJulianDate
    (daysFromJulianCivil valueYear valueMonth
      (nthJulianDayOfMonth nth target valueMonth valueYear))

||| Validate an nth-weekday request for a Julian month.
public export
refineNthDay : DayNth -> DayOfWeek -> JulianMonth -> Year ->
                     Either JulianDateError (CalendarDate Julian)
refineNthDay nth target valueMonth valueYear =
  case choose (isValidNthDay nth target valueMonth valueYear) of
    Left valid => Right (fromNthDay nth target valueMonth valueYear @{valid})
    Right _ => Left (InvalidJulianNthDay nth target valueMonth valueYear)

weekDateDays : WeekNumber -> DayOfWeek -> Year -> Integer
weekDateDays week target valueYear =
  let firstDay = daysFromJulianCivil valueYear JulianMonths.January 1
      firstWeekStart = firstDay -
        weekdayNumber (julianDayOfWeek (makeJulianDate firstDay))
   in firstWeekStart + 7 * (weekNumberValue week - 1) +
      weekdayNumber target

public export
isValidWeekDate : WeekNumber -> DayOfWeek -> Year -> Bool
isValidWeekDate week target valueYear =
  (yearValue valueYear > -44 && weekNumberValue week >= 0) ||
    IotaTime.Calendar.isValidDays {calendar = Julian}
      (weekDateDays week target valueYear)

||| Construct a Julian Sunday-based week date under static validity evidence.
public export
fromWeekDate : (week : WeekNumber) -> (target : DayOfWeek) ->
               (valueYear : Year) ->
               {auto 0 valid : So (isValidWeekDate week target valueYear)} ->
               CalendarDate Julian
fromWeekDate week target valueYear =
  makeJulianDate (weekDateDays week target valueYear)

||| Validate a runtime Julian Sunday-based week date.
public export
refineWeekDate : WeekNumber -> DayOfWeek -> Year ->
                 Either JulianDateError (CalendarDate Julian)
refineWeekDate week target valueYear =
  case choose (isValidWeekDate week target valueYear) of
    Left valid => Right (fromWeekDate week target valueYear @{valid})
    Right _ => Left (InvalidJulianWeekDate week target valueYear)