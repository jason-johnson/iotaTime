module IotaTime.Calendar.Julian

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

namespace JulianWeekdays
  public export
  data JulianDayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

  public export
  weekdayNumber : JulianDayOfWeek -> Integer
  weekdayNumber Sunday = 0
  weekdayNumber Monday = 1
  weekdayNumber Tuesday = 2
  weekdayNumber Wednesday = 3
  weekdayNumber Thursday = 4
  weekdayNumber Friday = 5
  weekdayNumber Saturday = 6

  public export
  Eq JulianDayOfWeek where
    left == right = weekdayNumber left == weekdayNumber right

  public export
  Ord JulianDayOfWeek where
    compare left right = compare (weekdayNumber left) (weekdayNumber right)

  %runElab derive `{JulianDayOfWeek} [Show]

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

weekdayFromNumber : Integer -> JulianDayOfWeek
weekdayFromNumber value = case value `mod` 7 of
  0 => JulianWeekdays.Sunday
  1 => JulianWeekdays.Monday
  2 => JulianWeekdays.Tuesday
  3 => JulianWeekdays.Wednesday
  4 => JulianWeekdays.Thursday
  5 => JulianWeekdays.Friday
  _ => JulianWeekdays.Saturday

export
record JulianDate where
  constructor MkJulianDate
  daysSinceEpoch : Integer

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

public export
HasCalendarDate JulianDate where
  calendarDays date = date.daysSinceEpoch + 13
  acceptsCalendarDays days = days - 13 >= epochDay
  calendarDateFromDays days = MkJulianDate (days - 13)
  calendarDateName = "Julian"

public export
isValidDate : DayOfMonth -> JulianMonth -> Year -> Bool
isValidDate valueDay valueMonth valueYear =
  dayOfMonthValue valueDay <= dayOfMonthValue (maxDaysInMonth valueMonth valueYear) &&
  yearValue valueYear >= -44

clampToJulian : Integer -> Integer
clampToJulian = max epochDay

shiftJulianDays : Integer -> JulianDate -> JulianDate
shiftJulianDays amount date = MkJulianDate (clampToJulian (date.daysSinceEpoch + amount))

shiftJulianMonths : Integer -> JulianDate -> JulianDate
shiftJulianMonths amount date =
  let (valueYear, valueMonth, valueDay) = julianCivilFromDays date.daysSinceEpoch
      monthOrdinal = JulianMonths.monthNumber valueMonth - 1 + amount
      targetYear = yearFromInteger (yearValue valueYear + monthOrdinal `div` 12)
      targetMonth = monthFromNumber (monthOrdinal `mod` 12 + 1)
      targetDay = min valueDay (maxDaysInMonth targetMonth targetYear)
   in MkJulianDate (clampToJulian (daysFromJulianCivil targetYear targetMonth targetDay))

shiftJulianYears : Integer -> JulianDate -> JulianDate
shiftJulianYears amount date =
  let (valueYear, valueMonth, valueDay) = julianCivilFromDays date.daysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + amount)
      targetDay = min valueDay (maxDaysInMonth valueMonth targetYear)
   in MkJulianDate (clampToJulian (daysFromJulianCivil targetYear valueMonth targetDay))

applyJulianPeriod : Period target -> JulianDate -> JulianDate
applyJulianPeriod period =
    shiftJulianDays (periodDays period)
  . shiftJulianDays (7 * periodWeeks period)
  . shiftJulianMonths (periodMonths period)
  . shiftJulianYears (periodYears period)

julianDayOfWeek : JulianDate -> JulianDayOfWeek
julianDayOfWeek date = weekdayFromNumber (date.daysSinceEpoch + 2)

nextJulian : Integer -> JulianDayOfWeek -> JulianDate -> JulianDate
nextJulian count target date =
  let current = JulianWeekdays.weekdayNumber (julianDayOfWeek date)
      wanted = JulianWeekdays.weekdayNumber target
      weeks = if wanted > current then count - 1 else count
   in MkJulianDate (clampToJulian (date.daysSinceEpoch + 7 * weeks + wanted - current))

previousJulian : Integer -> JulianDayOfWeek -> JulianDate -> JulianDate
previousJulian count target date =
  let current = JulianWeekdays.weekdayNumber (julianDayOfWeek date)
      wanted = JulianWeekdays.weekdayNumber target
      weeks = if wanted < current then count - 1 else count
   in MkJulianDate
        (clampToJulian (date.daysSinceEpoch - (7 * weeks + current - wanted)))

public export
Calendar Julian where
  DateRep = JulianDate
  MonthRep _ = JulianMonth
  WeekdayRep = JulianDayOfWeek

  isValidDays = (>= epochDay)
  fromDays days = MkJulianDate days
  toDays date = date.daysSinceEpoch
  calendarName = "Julian"

  year' date = let (value, _, _) = julianCivilFromDays date.daysSinceEpoch in value
  toYmd date = let (_, valueMonth, valueDay) = julianCivilFromDays date.daysSinceEpoch
                in (valueMonth, valueDay)
  day' date = let (_, _, value) = julianCivilFromDays date.daysSinceEpoch in value
  month' date = let (_, value, _) = julianCivilFromDays date.daysSinceEpoch in value

  applyCalendarPeriod' = applyJulianPeriod
  shiftCalendarDays' = shiftJulianDays

  dayOfWeek = julianDayOfWeek
  next = nextJulian
  previous = previousJulian

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
ApplyPeriod JulianDate where
  applyPeriod = applyJulianPeriod

public export
CalendarValue JulianDate where
  CalendarMonth _ = JulianMonth
  calendarValueYearMonthDay = yearMonthDayFor {calendar = Julian}
  calendarValueBetweenWith = betweenWithFor {calendar = Julian}

||| Construct a statically validated Julian date.
public export
calendarDate : (valueDay : DayOfMonth) -> (valueMonth : JulianMonth) ->
             (valueYear : Year) ->
             {auto 0 valid : So (isValidDate valueDay valueMonth valueYear)} ->
             CalendarDate Julian
calendarDate valueDay valueMonth valueYear =
  MkJulianDate (daysFromJulianCivil valueYear valueMonth valueDay)

||| Failures produced while refining untrusted Julian date data.
public export
data JulianDateError
  = InvalidJulianDate DayOfMonth JulianMonth Year
  | InvalidJulianDayCount Integer
  | InvalidJulianNthDay DayNth JulianDayOfWeek JulianMonth Year
  | InvalidJulianWeekDate WeekNumber JulianDayOfWeek Year

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
fromDays days = MkJulianDate days

||| Validate a runtime Julian day count.
public export
refineDays : Integer -> Either JulianDateError (CalendarDate Julian)
refineDays days = case choose
  (IotaTime.Calendar.isValidDays {calendar = Julian} days) of
  Left valid => Right (fromDays days @{valid})
  Right _ => Left (InvalidJulianDayCount days)

nthJulianDayOfMonth : DayNth -> JulianDayOfWeek -> JulianMonth -> Year -> DayOfMonth
nthJulianDayOfMonth nth target valueMonth valueYear =
  let monthLength = maxDaysInMonth valueMonth valueYear
      firstDate = MkJulianDate (daysFromJulianCivil valueYear valueMonth 1)
      firstOffset =
        (JulianWeekdays.weekdayNumber target -
         JulianWeekdays.weekdayNumber (julianDayOfWeek firstDate))
           `mod` daysPerWeek
      lastDate = MkJulianDate (daysFromJulianCivil valueYear valueMonth monthLength)
      lastOffset =
        (JulianWeekdays.weekdayNumber (julianDayOfWeek lastDate) -
         JulianWeekdays.weekdayNumber target) `mod` daysPerWeek
      dayNumber = nthWeekdayDayNumber nth (dayOfMonthValue monthLength)
        firstOffset lastOffset
   in dayOfMonthFromInteger dayNumber

public export
isValidNthDay : DayNth -> JulianDayOfWeek -> JulianMonth -> Year -> Bool
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
fromNthDay : (nth : DayNth) -> (target : JulianDayOfWeek) ->
                   (valueMonth : JulianMonth) -> (valueYear : Year) ->
                   {auto 0 valid : So
                     (isValidNthDay nth target valueMonth valueYear)} ->
                   CalendarDate Julian
fromNthDay nth target valueMonth valueYear =
  MkJulianDate
    (daysFromJulianCivil valueYear valueMonth
      (nthJulianDayOfMonth nth target valueMonth valueYear))

||| Validate an nth-weekday request for a Julian month.
public export
refineNthDay : DayNth -> JulianDayOfWeek -> JulianMonth -> Year ->
                     Either JulianDateError (CalendarDate Julian)
refineNthDay nth target valueMonth valueYear =
  case choose (isValidNthDay nth target valueMonth valueYear) of
    Left valid => Right (fromNthDay nth target valueMonth valueYear @{valid})
    Right _ => Left (InvalidJulianNthDay nth target valueMonth valueYear)

weekDateDays : WeekNumber -> JulianDayOfWeek -> Year -> Integer
weekDateDays week target valueYear =
  let firstDay = daysFromJulianCivil valueYear JulianMonths.January 1
      firstWeekStart = firstDay -
        JulianWeekdays.weekdayNumber (julianDayOfWeek (MkJulianDate firstDay))
   in firstWeekStart + 7 * (weekNumberValue week - 1) +
      JulianWeekdays.weekdayNumber target

public export
isValidWeekDate : WeekNumber -> JulianDayOfWeek -> Year -> Bool
isValidWeekDate week target valueYear =
  (yearValue valueYear > -44 && weekNumberValue week >= 0) ||
    IotaTime.Calendar.isValidDays {calendar = Julian}
      (weekDateDays week target valueYear)

||| Construct a Julian Sunday-based week date under static validity evidence.
public export
fromWeekDate : (week : WeekNumber) -> (target : JulianDayOfWeek) ->
               (valueYear : Year) ->
               {auto 0 valid : So (isValidWeekDate week target valueYear)} ->
               CalendarDate Julian
fromWeekDate week target valueYear =
  MkJulianDate (weekDateDays week target valueYear)

||| Validate a runtime Julian Sunday-based week date.
public export
refineWeekDate : WeekNumber -> JulianDayOfWeek -> Year ->
                 Either JulianDateError (CalendarDate Julian)
refineWeekDate week target valueYear =
  case choose (isValidWeekDate week target valueYear) of
    Left valid => Right (fromWeekDate week target valueYear @{valid})
    Right _ => Left (InvalidJulianWeekDate week target valueYear)