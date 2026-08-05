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

export
Eq JulianDate where
  left == right = left.daysSinceEpoch == right.daysSinceEpoch

export
Ord JulianDate where
  compare left right = compare left.daysSinceEpoch right.daysSinceEpoch

||| Whether a Julian year is divisible by four and therefore leap.
public export
isJulianLeapYear : Year -> Bool
isJulianLeapYear value = yearValue value `mod` 4 == 0

public export
maxJulianDaysInMonth : JulianMonth -> Year -> DayOfMonth
maxJulianDaysInMonth JulianMonths.February value = if isJulianLeapYear value then 29 else 28
maxJulianDaysInMonth JulianMonths.April _ = 30
maxJulianDaysInMonth JulianMonths.June _ = 30
maxJulianDaysInMonth JulianMonths.September _ = 30
maxJulianDaysInMonth JulianMonths.November _ = 30
maxJulianDaysInMonth _ _ = 31

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

firstJulianDay : Integer
firstJulianDay = -746631

public export
isValidJulianDays : Integer -> Bool
isValidJulianDays value = value >= -746631

public export
HasCalendarDate JulianDate where
  calendarDays date = date.daysSinceEpoch + 13
  acceptsCalendarDays days = isValidJulianDays (days - 13)
  calendarDateFromDays days = MkJulianDate (days - 13)
  calendarDateName = "Julian"

public export
isValidJulianDate : DayOfMonth -> JulianMonth -> Year -> Bool
isValidJulianDate valueDay valueMonth valueYear =
  dayOfMonthValue valueDay <= dayOfMonthValue (maxJulianDaysInMonth valueMonth valueYear) &&
  yearValue valueYear >= -44

clampToJulian : Integer -> Integer
clampToJulian = max firstJulianDay

shiftJulianDays : Integer -> JulianDate -> JulianDate
shiftJulianDays amount date = MkJulianDate (clampToJulian (date.daysSinceEpoch + amount))

shiftJulianMonths : Integer -> JulianDate -> JulianDate
shiftJulianMonths amount date =
  let (valueYear, valueMonth, valueDay) = julianCivilFromDays date.daysSinceEpoch
      monthOrdinal = JulianMonths.monthNumber valueMonth - 1 + amount
      targetYear = yearFromInteger (yearValue valueYear + monthOrdinal `div` 12)
      targetMonth = monthFromNumber (monthOrdinal `mod` 12 + 1)
      targetDay = min valueDay (maxJulianDaysInMonth targetMonth targetYear)
   in MkJulianDate (clampToJulian (daysFromJulianCivil targetYear targetMonth targetDay))

shiftJulianYears : Integer -> JulianDate -> JulianDate
shiftJulianYears amount date =
  let (valueYear, valueMonth, valueDay) = julianCivilFromDays date.daysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + amount)
      targetDay = min valueDay (maxJulianDaysInMonth valueMonth targetYear)
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

  isValidDays = isValidJulianDays
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
HasCalendar JulianDate where
  calendarCapability = ()

public export
ApplyPeriod JulianDate where
  applyPeriod = applyJulianPeriod

||| Construct a statically validated Julian date.
public export
julianDate : (valueDay : DayOfMonth) -> (valueMonth : JulianMonth) ->
             (valueYear : Year) ->
             {auto 0 valid : So (isValidJulianDate valueDay valueMonth valueYear)} ->
             CalendarDate Julian
julianDate valueDay valueMonth valueYear =
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
refineJulianDate : DayOfMonth -> JulianMonth -> Year ->
                   Either JulianDateError (CalendarDate Julian)
refineJulianDate valueDay valueMonth valueYear =
  case choose (isValidJulianDate valueDay valueMonth valueYear) of
    Left valid => Right (julianDate valueDay valueMonth valueYear @{valid})
    Right _ => Left (InvalidJulianDate valueDay valueMonth valueYear)

||| Construct a Julian date from a statically valid calendar-relative day count.
public export
julianFromDays : (days : Integer) -> {auto 0 valid : So (isValidJulianDays days)} ->
                 CalendarDate Julian
julianFromDays days = MkJulianDate days

||| Validate a runtime Julian day count.
public export
refineJulianDays : Integer -> Either JulianDateError (CalendarDate Julian)
refineJulianDays days = case choose (isValidJulianDays days) of
  Left valid => Right (julianFromDays days @{valid})
  Right _ => Left (InvalidJulianDayCount days)

nthJulianDayOfMonth : DayNth -> JulianDayOfWeek -> JulianMonth -> Year -> DayOfMonth
nthJulianDayOfMonth nth target valueMonth valueYear =
  let monthLength = maxJulianDaysInMonth valueMonth valueYear
      firstDate = MkJulianDate (daysFromJulianCivil valueYear valueMonth 1)
      firstOffset =
        (JulianWeekdays.weekdayNumber target -
         JulianWeekdays.weekdayNumber (julianDayOfWeek firstDate)) `mod` 7
      lastDate = MkJulianDate (daysFromJulianCivil valueYear valueMonth monthLength)
      lastOffset =
        (JulianWeekdays.weekdayNumber (julianDayOfWeek lastDate) -
         JulianWeekdays.weekdayNumber target) `mod` 7
      dayNumber = case nth of
        First => 1 + firstOffset
        Second => 8 + firstOffset
        Third => 15 + firstOffset
        Fourth => 22 + firstOffset
        Fifth => 29 + firstOffset
        Last => dayOfMonthValue monthLength - lastOffset
   in dayOfMonthFromInteger dayNumber

public export
isValidJulianNthDay : DayNth -> JulianDayOfWeek -> JulianMonth -> Year -> Bool
isValidJulianNthDay nth target valueMonth valueYear =
  if yearValue valueYear > -44
    then case nth of
      Fifth => nthJulianDayOfMonth nth target valueMonth valueYear <=
        maxJulianDaysInMonth valueMonth valueYear
      _ => True
    else isValidJulianDate
      (nthJulianDayOfMonth nth target valueMonth valueYear) valueMonth valueYear

||| Construct the nth requested weekday in a Julian month.
public export
julianFromNthDay : (nth : DayNth) -> (target : JulianDayOfWeek) ->
                   (valueMonth : JulianMonth) -> (valueYear : Year) ->
                   {auto 0 valid : So
                     (isValidJulianNthDay nth target valueMonth valueYear)} ->
                   CalendarDate Julian
julianFromNthDay nth target valueMonth valueYear =
  MkJulianDate
    (daysFromJulianCivil valueYear valueMonth
      (nthJulianDayOfMonth nth target valueMonth valueYear))

||| Validate an nth-weekday request for a Julian month.
public export
refineJulianNthDay : DayNth -> JulianDayOfWeek -> JulianMonth -> Year ->
                     Either JulianDateError (CalendarDate Julian)
refineJulianNthDay nth target valueMonth valueYear =
  case choose (isValidJulianNthDay nth target valueMonth valueYear) of
    Left valid => Right (julianFromNthDay nth target valueMonth valueYear @{valid})
    Right _ => Left (InvalidJulianNthDay nth target valueMonth valueYear)

julianWeekDateDays : WeekNumber -> JulianDayOfWeek -> Year -> Integer
julianWeekDateDays week target valueYear =
  let firstDay = daysFromJulianCivil valueYear JulianMonths.January 1
      firstWeekStart = firstDay -
        JulianWeekdays.weekdayNumber (julianDayOfWeek (MkJulianDate firstDay))
   in firstWeekStart + 7 * (weekNumberValue week - 1) +
      JulianWeekdays.weekdayNumber target

public export
isValidJulianWeekDate : WeekNumber -> JulianDayOfWeek -> Year -> Bool
isValidJulianWeekDate week target valueYear =
  yearValue valueYear > -44 ||
  isValidJulianDays (julianWeekDateDays week target valueYear)

||| Construct a Julian Sunday-based week date under static validity evidence.
public export
julianFromWeekDate : (week : WeekNumber) -> (target : JulianDayOfWeek) ->
                     (valueYear : Year) ->
                     {auto 0 valid : So (isValidJulianWeekDate week target valueYear)} ->
                     CalendarDate Julian
julianFromWeekDate week target valueYear =
  MkJulianDate (julianWeekDateDays week target valueYear)

||| Validate a runtime Julian Sunday-based week date.
public export
refineJulianWeekDate : WeekNumber -> JulianDayOfWeek -> Year ->
                       Either JulianDateError (CalendarDate Julian)
refineJulianWeekDate week target valueYear =
  case choose (isValidJulianWeekDate week target valueYear) of
    Left valid => Right (julianFromWeekDate week target valueYear @{valid})
    Right _ => Left (InvalidJulianWeekDate week target valueYear)