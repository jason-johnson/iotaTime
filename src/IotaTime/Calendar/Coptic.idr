module IotaTime.Calendar.Coptic

import IotaTime.Calendar
import IotaTime.Period
import Data.So
import Derive.Prelude

%language ElabReflection

%default total

||| The 13-month Coptic calendar, supported from 1 Thout 1.
public export
data Coptic = CopticCalendar

namespace CopticMonths
  public export
  data CopticMonth
    = Thout | Paopi | Hathor | Koiak | Tobi | Meshir | Paremhat
    | Paremoude | Pashons | Paoni | Epip | Mesori | PiKogiEnavot

  public export
  monthNumber : CopticMonth -> Integer
  monthNumber Thout = 1
  monthNumber Paopi = 2
  monthNumber Hathor = 3
  monthNumber Koiak = 4
  monthNumber Tobi = 5
  monthNumber Meshir = 6
  monthNumber Paremhat = 7
  monthNumber Paremoude = 8
  monthNumber Pashons = 9
  monthNumber Paoni = 10
  monthNumber Epip = 11
  monthNumber Mesori = 12
  monthNumber PiKogiEnavot = 13

  public export
  Eq CopticMonth where
    left == right = monthNumber left == monthNumber right

  public export
  Ord CopticMonth where
    compare left right = compare (monthNumber left) (monthNumber right)

  %runElab derive `{CopticMonth} [Show]

namespace CopticWeekdays
  public export
  data CopticDayOfWeek
    = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

  public export
  weekdayNumber : CopticDayOfWeek -> Integer
  weekdayNumber Sunday = 0
  weekdayNumber Monday = 1
  weekdayNumber Tuesday = 2
  weekdayNumber Wednesday = 3
  weekdayNumber Thursday = 4
  weekdayNumber Friday = 5
  weekdayNumber Saturday = 6

  public export
  Eq CopticDayOfWeek where
    left == right = weekdayNumber left == weekdayNumber right

  public export
  Ord CopticDayOfWeek where
    compare left right = compare (weekdayNumber left) (weekdayNumber right)

  %runElab derive `{CopticDayOfWeek} [Show]

monthFromNumber : Integer -> CopticMonth
monthFromNumber 1 = CopticMonths.Thout
monthFromNumber 2 = CopticMonths.Paopi
monthFromNumber 3 = CopticMonths.Hathor
monthFromNumber 4 = CopticMonths.Koiak
monthFromNumber 5 = CopticMonths.Tobi
monthFromNumber 6 = CopticMonths.Meshir
monthFromNumber 7 = CopticMonths.Paremhat
monthFromNumber 8 = CopticMonths.Paremoude
monthFromNumber 9 = CopticMonths.Pashons
monthFromNumber 10 = CopticMonths.Paoni
monthFromNumber 11 = CopticMonths.Epip
monthFromNumber 12 = CopticMonths.Mesori
monthFromNumber _ = CopticMonths.PiKogiEnavot

weekdayFromNumber : Integer -> CopticDayOfWeek
weekdayFromNumber value = case value `mod` 7 of
  0 => CopticWeekdays.Sunday
  1 => CopticWeekdays.Monday
  2 => CopticWeekdays.Tuesday
  3 => CopticWeekdays.Wednesday
  4 => CopticWeekdays.Thursday
  5 => CopticWeekdays.Friday
  _ => CopticWeekdays.Saturday

export
record CopticDate where
  constructor MkCopticDate
  daysSinceEpoch : Integer

public export
Eq CopticDate where
  left == right = left.daysSinceEpoch == right.daysSinceEpoch

public export
Ord CopticDate where
  compare left right = compare left.daysSinceEpoch right.daysSinceEpoch

||| The Coptic calendar epoch day relative to March 1, 2000 Gregorian.
public export
epochDay : Integer
epochDay = -626575

||| Whether a Coptic year has a sixth epagomenal day.
public export
isLeapYear : Year -> Bool
isLeapYear value = yearValue value `mod` 4 == 3

public export
maxDaysInMonth : CopticMonth -> Year -> DayOfMonth
maxDaysInMonth CopticMonths.PiKogiEnavot value =
  if isLeapYear value then 6 else 5
maxDaysInMonth _ _ = 30

public export
isValidDate : DayOfMonth -> CopticMonth -> Year -> Bool
isValidDate valueDay CopticMonths.PiKogiEnavot valueYear =
  let dayNumber = dayOfMonthValue valueDay
      yearNumber = yearValue valueYear
   in dayNumber >= 1 &&
      dayNumber <= (if yearNumber `mod` 4 == 3 then 6 else 5) &&
      yearNumber >= 1
isValidDate valueDay _ valueYear =
  let dayNumber = dayOfMonthValue valueDay
   in dayNumber >= 1 && dayNumber <= 30 && yearValue valueYear >= 1

copticDaysFromCivil : Year -> CopticMonth -> DayOfMonth -> Integer
copticDaysFromCivil valueYear valueMonth valueDay =
  epochDay + (yearValue valueYear - 1) * 365 +
    yearValue valueYear `div` 4 +
    (CopticMonths.monthNumber valueMonth - 1) * 30 +
    dayOfMonthValue valueDay - 1

copticCivilFromDays : Integer -> (Year, CopticMonth, DayOfMonth)
copticCivilFromDays value =
  let relative = value - epochDay
      cycle = relative `div` 1461
      remaining = relative - cycle * 1461
      yearInCycle = if remaining < 365 then 0
        else if remaining < 730 then 1
        else if remaining < 1096 then 2
        else 3
      beforeYear = if yearInCycle == 0 then 0
        else if yearInCycle == 1 then 365
        else if yearInCycle == 2 then 730
        else 1096
      dayOfYear = remaining - beforeYear
      yearNumber = cycle * 4 + yearInCycle + 1
      monthNumber = if dayOfYear >= 360 then 13 else dayOfYear `div` 30 + 1
      dayNumber = if dayOfYear >= 360 then dayOfYear - 359
        else dayOfYear `mod` 30 + 1
   in (yearFromInteger yearNumber, monthFromNumber monthNumber,
       dayOfMonthFromInteger dayNumber)

public export
HasCalendarDate CopticDate where
  calendarDays = daysSinceEpoch
  acceptsCalendarDays = (>= epochDay)
  calendarDateFromDays days = MkCopticDate days
  calendarDateName = "Coptic"

clampToCoptic : Integer -> Integer
clampToCoptic = max epochDay

shiftCopticDays : Integer -> CopticDate -> CopticDate
shiftCopticDays amount date =
  MkCopticDate (clampToCoptic (date.daysSinceEpoch + amount))

shiftCopticMonths : Integer -> CopticDate -> CopticDate
shiftCopticMonths amount date =
  let (valueYear, valueMonth, valueDay) = copticCivilFromDays date.daysSinceEpoch
      monthOrdinal = CopticMonths.monthNumber valueMonth - 1 + amount
      targetYear = yearFromInteger (yearValue valueYear + monthOrdinal `div` 13)
      targetMonth = monthFromNumber (monthOrdinal `mod` 13 + 1)
      targetDay = min valueDay (maxDaysInMonth targetMonth targetYear)
   in MkCopticDate (clampToCoptic
        (copticDaysFromCivil targetYear targetMonth targetDay))

shiftCopticYears : Integer -> CopticDate -> CopticDate
shiftCopticYears amount date =
  let (valueYear, valueMonth, valueDay) = copticCivilFromDays date.daysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + amount)
      targetDay = min valueDay (maxDaysInMonth valueMonth targetYear)
   in MkCopticDate (clampToCoptic
        (copticDaysFromCivil targetYear valueMonth targetDay))

applyCopticPeriod : Period target -> CopticDate -> CopticDate
applyCopticPeriod period =
    shiftCopticDays (periodDays period)
  . shiftCopticDays (7 * periodWeeks period)
  . shiftCopticMonths (periodMonths period)
  . shiftCopticYears (periodYears period)

copticWeekdayFromDays : Integer -> CopticDayOfWeek
copticWeekdayFromDays days = weekdayFromNumber (days + 3)

copticDayOfWeek : CopticDate -> CopticDayOfWeek
copticDayOfWeek date = copticWeekdayFromDays date.daysSinceEpoch

nextCoptic : Integer -> CopticDayOfWeek -> CopticDate -> CopticDate
nextCoptic count target date =
  let current = CopticWeekdays.weekdayNumber (copticDayOfWeek date)
      wanted = CopticWeekdays.weekdayNumber target
      weeks = if wanted > current then count - 1 else count
   in MkCopticDate
        (clampToCoptic (date.daysSinceEpoch + 7 * weeks + wanted - current))

previousCoptic : Integer -> CopticDayOfWeek -> CopticDate -> CopticDate
previousCoptic count target date =
  let current = CopticWeekdays.weekdayNumber (copticDayOfWeek date)
      wanted = CopticWeekdays.weekdayNumber target
      weeks = if wanted < current then count - 1 else count
   in MkCopticDate (clampToCoptic
        (date.daysSinceEpoch - (7 * weeks + current - wanted)))

public export
Calendar Coptic where
  DateRep = CopticDate
  MonthRep _ = CopticMonth
  WeekdayRep = CopticDayOfWeek

  isValidDays = (>= epochDay)
  fromDays days = MkCopticDate days
  toDaysFor date = date.daysSinceEpoch
  calendarName = "Coptic"

  year' date = let (value, _, _) = copticCivilFromDays date.daysSinceEpoch in value
  toYmd date = let (_, valueMonth, valueDay) =
                    copticCivilFromDays date.daysSinceEpoch
                in (valueMonth, valueDay)
  day' date = let (_, _, value) = copticCivilFromDays date.daysSinceEpoch in value
  month' date = let (_, value, _) = copticCivilFromDays date.daysSinceEpoch in value

  applyCalendarPeriod' = applyCopticPeriod
  shiftCalendarDays' = shiftCopticDays

  dayOfWeekFor = copticDayOfWeek
  nextFor = nextCoptic
  previousFor = previousCoptic

public export
Show CopticDate where
  show date = case copticCivilFromDays date.daysSinceEpoch of
    (valueYear, valueMonth, valueDay) =>
      "calendarDate " ++ show valueDay ++ " " ++
      show valueMonth ++ " " ++ show valueYear

public export
HasCalendar CopticDate where
  calendarCapability = ()

public export
ApplyPeriod CopticDate where
  applyPeriod = applyCopticPeriod

public export
CalendarValue CopticDate where
  CalendarMonth _ = CopticMonth
  CalendarWeekday = CopticDayOfWeek
  calendarValueToDays = toDaysFor {calendar = Coptic}
  calendarValueYear = yearFor {calendar = Coptic}
  calendarValueMonthDay = toYmd {calendar = Coptic}
  calendarValueDayOfWeek = dayOfWeekFor {calendar = Coptic}
  calendarValueBetweenWith = betweenWithFor {calendar = Coptic}

public export
CalendarNavigation CopticDayOfWeek CopticDate where
  calendarValueNext = nextFor {calendar = Coptic}
  calendarValuePrevious = previousFor {calendar = Coptic}

||| Construct a statically validated Coptic date.
public export
calendarDate : (valueDay : DayOfMonth) -> (valueMonth : CopticMonth) ->
             (valueYear : Year) ->
             {auto 0 valid : So (isValidDate valueDay valueMonth valueYear)} ->
             CalendarDate Coptic
calendarDate valueDay valueMonth valueYear =
  MkCopticDate (copticDaysFromCivil valueYear valueMonth valueDay)

||| Failures produced while refining untrusted Coptic date data.
public export
data CopticDateError
  = InvalidCopticDate DayOfMonth CopticMonth Year
  | InvalidCopticDayCount Integer
  | InvalidCopticNthDay DayNth CopticDayOfWeek CopticMonth Year
  | InvalidCopticWeekDate WeekNumber CopticDayOfWeek Year

||| Validate runtime day, month, and year components as a Coptic date.
public export
refineDate : DayOfMonth -> CopticMonth -> Year ->
                   Either CopticDateError (CalendarDate Coptic)
refineDate valueDay valueMonth valueYear =
  case choose (isValidDate valueDay valueMonth valueYear) of
    Left valid => Right (calendarDate valueDay valueMonth valueYear @{valid})
    Right _ => Left (InvalidCopticDate valueDay valueMonth valueYear)

||| Construct a Coptic date from a statically valid calendar-relative day count.
public export
fromDays : (days : Integer) ->
                 {auto 0 valid : So
                   (IotaTime.Calendar.isValidDays {calendar = Coptic} days)} ->
                 CalendarDate Coptic
fromDays days = MkCopticDate days

||| Validate a runtime Coptic day count.
public export
refineDays : Integer -> Either CopticDateError (CalendarDate Coptic)
refineDays days = case choose
  (IotaTime.Calendar.isValidDays {calendar = Coptic} days) of
  Left valid => Right (fromDays days @{valid})
  Right _ => Left (InvalidCopticDayCount days)

copticNthDayNumber : DayNth -> CopticDayOfWeek -> CopticMonth -> Year -> Integer
copticNthDayNumber nth target valueMonth valueYear =
  let monthLength = maxDaysInMonth valueMonth valueYear
      firstOffset = (CopticWeekdays.weekdayNumber target -
        CopticWeekdays.weekdayNumber (copticWeekdayFromDays
          (copticDaysFromCivil valueYear valueMonth 1))) `mod` daysPerWeek
      lastOffset = (CopticWeekdays.weekdayNumber (copticWeekdayFromDays
        (copticDaysFromCivil valueYear valueMonth monthLength)) -
        CopticWeekdays.weekdayNumber target) `mod` daysPerWeek
   in nthWeekdayDayNumber nth (dayOfMonthValue monthLength)
        firstOffset lastOffset

public export
isValidNthDay : DayNth -> CopticDayOfWeek -> CopticMonth -> Year -> Bool
isValidNthDay nth target valueMonth valueYear =
  yearValue valueYear >= 1 && case valueMonth of
    CopticMonths.PiKogiEnavot =>
      let candidate = copticNthDayNumber nth target valueMonth valueYear
          monthLength = dayOfMonthValue
            (maxDaysInMonth valueMonth valueYear)
       in candidate >= 1 && candidate <= monthLength
    _ => case nth of
      Fifth => copticNthDayNumber nth target valueMonth valueYear <= 30
      _ => True

||| Return the requested weekday occurrence under static validity evidence.
public export
nthDayOfMonth : (nth : DayNth) -> (target : CopticDayOfWeek) ->
                      (valueMonth : CopticMonth) -> (valueYear : Year) ->
                      {auto 0 valid : So
                        (isValidNthDay nth target valueMonth valueYear)} ->
                      DayOfMonth
nthDayOfMonth nth target valueMonth valueYear =
  dayOfMonthFromInteger
    (copticNthDayNumber nth target valueMonth valueYear)

||| Construct the nth requested weekday in a Coptic month.
public export
fromNthDay : (nth : DayNth) -> (target : CopticDayOfWeek) ->
                   (valueMonth : CopticMonth) -> (valueYear : Year) ->
                   {auto 0 valid : So
                     (isValidNthDay nth target valueMonth valueYear)} ->
                   CalendarDate Coptic
fromNthDay nth target valueMonth valueYear =
  MkCopticDate (copticDaysFromCivil valueYear valueMonth
    (nthDayOfMonth nth target valueMonth valueYear))

||| Validate an nth-weekday request for a Coptic month.
public export
refineNthDay : DayNth -> CopticDayOfWeek -> CopticMonth -> Year ->
                     Either CopticDateError (CalendarDate Coptic)
refineNthDay nth target valueMonth valueYear =
  case choose (isValidNthDay nth target valueMonth valueYear) of
    Left valid => Right
      (fromNthDay nth target valueMonth valueYear @{valid})
    Right _ => Left (InvalidCopticNthDay nth target valueMonth valueYear)

public export
weekDateDays : WeekNumber -> CopticDayOfWeek -> Year -> Integer
weekDateDays week target valueYear =
  let firstDay = copticDaysFromCivil valueYear CopticMonths.Thout 1
      firstWeekStart = firstDay -
        CopticWeekdays.weekdayNumber (copticWeekdayFromDays firstDay)
   in firstWeekStart + 7 * (weekNumberValue week - 1) +
      CopticWeekdays.weekdayNumber target

public export
isValidWeekDate : WeekNumber -> CopticDayOfWeek -> Year -> Bool
isValidWeekDate week target valueYear =
  (yearValue valueYear > 1 && weekNumberValue week >= 0) ||
    IotaTime.Calendar.isValidDays {calendar = Coptic}
      (weekDateDays week target valueYear)

||| Construct a Coptic Sunday-based week date under static validity evidence.
public export
fromWeekDate : (week : WeekNumber) -> (target : CopticDayOfWeek) ->
               (valueYear : Year) ->
               {auto 0 valid : So (isValidWeekDate week target valueYear)} ->
               CalendarDate Coptic
fromWeekDate week target valueYear =
  MkCopticDate (weekDateDays week target valueYear)

||| Validate a runtime Coptic Sunday-based week date.
public export
refineWeekDate : WeekNumber -> CopticDayOfWeek -> Year ->
                 Either CopticDateError (CalendarDate Coptic)
refineWeekDate week target valueYear =
  case choose (isValidWeekDate week target valueYear) of
    Left valid => Right (fromWeekDate week target valueYear @{valid})
    Right _ => Left (InvalidCopticWeekDate week target valueYear)
