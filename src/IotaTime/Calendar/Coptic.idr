module IotaTime.Calendar.Coptic

import IotaTime.Calendar
import IotaTime.Period
import Data.So

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

  public export
  Show CopticMonth where
    show Thout = "Thout"
    show Paopi = "Paopi"
    show Hathor = "Hathor"
    show Koiak = "Koiak"
    show Tobi = "Tobi"
    show Meshir = "Meshir"
    show Paremhat = "Paremhat"
    show Paremoude = "Paremoude"
    show Pashons = "Pashons"
    show Paoni = "Paoni"
    show Epip = "Epip"
    show Mesori = "Mesori"
    show PiKogiEnavot = "PiKogiEnavot"

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

  public export
  Show CopticDayOfWeek where
    show Sunday = "Sunday"
    show Monday = "Monday"
    show Tuesday = "Tuesday"
    show Wednesday = "Wednesday"
    show Thursday = "Thursday"
    show Friday = "Friday"
    show Saturday = "Saturday"

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

export
Eq CopticDate where
  left == right = left.daysSinceEpoch == right.daysSinceEpoch

export
Ord CopticDate where
  compare left right = compare left.daysSinceEpoch right.daysSinceEpoch

copticEpoch : Integer
copticEpoch = -626575

||| Whether a Coptic year has a sixth epagomenal day.
public export
isCopticLeapYear : Year -> Bool
isCopticLeapYear value = yearValue value `mod` 4 == 3

public export
maxCopticDaysInMonth : CopticMonth -> Year -> DayOfMonth
maxCopticDaysInMonth CopticMonths.PiKogiEnavot value =
  if isCopticLeapYear value then 6 else 5
maxCopticDaysInMonth _ _ = 30

public export
isValidCopticDate : DayOfMonth -> CopticMonth -> Year -> Bool
isValidCopticDate valueDay CopticMonths.PiKogiEnavot valueYear =
  let dayNumber = dayOfMonthValue valueDay
      yearNumber = yearValue valueYear
   in dayNumber >= 1 &&
      dayNumber <= (if yearNumber `mod` 4 == 3 then 6 else 5) &&
      yearNumber >= 1
isValidCopticDate valueDay _ valueYear =
  let dayNumber = dayOfMonthValue valueDay
   in dayNumber >= 1 && dayNumber <= 30 && yearValue valueYear >= 1

copticDaysFromCivil : Year -> CopticMonth -> DayOfMonth -> Integer
copticDaysFromCivil valueYear valueMonth valueDay =
  copticEpoch + (yearValue valueYear - 1) * 365 +
    yearValue valueYear `div` 4 +
    (CopticMonths.monthNumber valueMonth - 1) * 30 +
    dayOfMonthValue valueDay - 1

copticCivilFromDays : Integer -> (Year, CopticMonth, DayOfMonth)
copticCivilFromDays value =
  let relative = value - copticEpoch
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
isValidCopticDays : Integer -> Bool
isValidCopticDays value = value >= copticEpoch

public export
HasCalendarDate CopticDate where
  calendarDays = daysSinceEpoch
  acceptsCalendarDays = isValidCopticDays
  calendarDateFromDays days = MkCopticDate days
  calendarDateName = "Coptic"

clampToCoptic : Integer -> Integer
clampToCoptic = max copticEpoch

shiftCopticDays : Integer -> CopticDate -> CopticDate
shiftCopticDays amount date =
  MkCopticDate (clampToCoptic (date.daysSinceEpoch + amount))

shiftCopticMonths : Integer -> CopticDate -> CopticDate
shiftCopticMonths amount date =
  let (valueYear, valueMonth, valueDay) = copticCivilFromDays date.daysSinceEpoch
      monthOrdinal = CopticMonths.monthNumber valueMonth - 1 + amount
      targetYear = yearFromInteger (yearValue valueYear + monthOrdinal `div` 13)
      targetMonth = monthFromNumber (monthOrdinal `mod` 13 + 1)
      targetDay = min valueDay (maxCopticDaysInMonth targetMonth targetYear)
   in MkCopticDate (clampToCoptic
        (copticDaysFromCivil targetYear targetMonth targetDay))

shiftCopticYears : Integer -> CopticDate -> CopticDate
shiftCopticYears amount date =
  let (valueYear, valueMonth, valueDay) = copticCivilFromDays date.daysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + amount)
      targetDay = min valueDay (maxCopticDaysInMonth valueMonth targetYear)
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

  isValidDays = isValidCopticDays
  fromDays days = MkCopticDate days
  toDays date = date.daysSinceEpoch
  calendarName = "Coptic"

  year' date = let (value, _, _) = copticCivilFromDays date.daysSinceEpoch in value
  toYmd date = let (_, valueMonth, valueDay) =
                    copticCivilFromDays date.daysSinceEpoch
                in (valueMonth, valueDay)
  day' date = let (_, _, value) = copticCivilFromDays date.daysSinceEpoch in value
  month' date = let (_, value, _) = copticCivilFromDays date.daysSinceEpoch in value

  applyCalendarPeriod' = applyCopticPeriod
  shiftCalendarDays' = shiftCopticDays

  dayOfWeek = copticDayOfWeek
  next = nextCoptic
  previous = previousCoptic

public export
HasCalendar CopticDate where
  calendarCapability = ()

public export
ApplyPeriod CopticDate where
  applyPeriod = applyCopticPeriod

||| Construct a statically validated Coptic date.
public export
copticDate : (valueDay : DayOfMonth) -> (valueMonth : CopticMonth) ->
             (valueYear : Year) ->
             {auto 0 valid : So (isValidCopticDate valueDay valueMonth valueYear)} ->
             CalendarDate Coptic
copticDate valueDay valueMonth valueYear =
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
refineCopticDate : DayOfMonth -> CopticMonth -> Year ->
                   Either CopticDateError (CalendarDate Coptic)
refineCopticDate valueDay valueMonth valueYear =
  case choose (isValidCopticDate valueDay valueMonth valueYear) of
    Left valid => Right (copticDate valueDay valueMonth valueYear @{valid})
    Right _ => Left (InvalidCopticDate valueDay valueMonth valueYear)

||| Construct a Coptic date from a statically valid calendar-relative day count.
public export
copticFromDays : (days : Integer) ->
                 {auto 0 valid : So (isValidCopticDays days)} ->
                 CalendarDate Coptic
copticFromDays days = MkCopticDate days

||| Validate a runtime Coptic day count.
public export
refineCopticDays : Integer -> Either CopticDateError (CalendarDate Coptic)
refineCopticDays days = case choose (isValidCopticDays days) of
  Left valid => Right (copticFromDays days @{valid})
  Right _ => Left (InvalidCopticDayCount days)

public export
nthCopticDayOfMonth : DayNth -> CopticDayOfWeek -> CopticMonth -> Year -> DayOfMonth
nthCopticDayOfMonth nth target valueMonth valueYear =
  let monthLength = maxCopticDaysInMonth valueMonth valueYear
      firstOffset = (CopticWeekdays.weekdayNumber target -
        CopticWeekdays.weekdayNumber (copticWeekdayFromDays
          (copticDaysFromCivil valueYear valueMonth 1))) `mod` 7
      lastOffset = (CopticWeekdays.weekdayNumber (copticWeekdayFromDays
        (copticDaysFromCivil valueYear valueMonth monthLength)) -
        CopticWeekdays.weekdayNumber target) `mod` 7
      dayNumber = case nth of
        First => 1 + firstOffset
        Second => 8 + firstOffset
        Third => 15 + firstOffset
        Fourth => 22 + firstOffset
        Fifth => 29 + firstOffset
        Last => dayOfMonthValue monthLength - lastOffset
   in dayOfMonthFromInteger dayNumber

public export
isValidCopticNthDay : DayNth -> CopticDayOfWeek -> CopticMonth -> Year -> Bool
isValidCopticNthDay nth target valueMonth valueYear =
  yearValue valueYear >= 1 && case valueMonth of
    CopticMonths.PiKogiEnavot =>
      nthCopticDayOfMonth nth target valueMonth valueYear <=
        maxCopticDaysInMonth valueMonth valueYear
    _ => case nth of
      Fifth => nthCopticDayOfMonth nth target valueMonth valueYear <= 30
      _ => True

||| Construct the nth requested weekday in a Coptic month.
public export
copticFromNthDay : (nth : DayNth) -> (target : CopticDayOfWeek) ->
                   (valueMonth : CopticMonth) -> (valueYear : Year) ->
                   {auto 0 valid : So
                     (isValidCopticNthDay nth target valueMonth valueYear)} ->
                   CalendarDate Coptic
copticFromNthDay nth target valueMonth valueYear =
  MkCopticDate (copticDaysFromCivil valueYear valueMonth
    (nthCopticDayOfMonth nth target valueMonth valueYear))

||| Validate an nth-weekday request for a Coptic month.
public export
refineCopticNthDay : DayNth -> CopticDayOfWeek -> CopticMonth -> Year ->
                     Either CopticDateError (CalendarDate Coptic)
refineCopticNthDay nth target valueMonth valueYear =
  case choose (isValidCopticNthDay nth target valueMonth valueYear) of
    Left valid => Right
      (copticFromNthDay nth target valueMonth valueYear @{valid})
    Right _ => Left (InvalidCopticNthDay nth target valueMonth valueYear)

public export
copticWeekDateDays : WeekNumber -> CopticDayOfWeek -> Year -> Integer
copticWeekDateDays week target valueYear =
  let firstDay = copticDaysFromCivil valueYear CopticMonths.Thout 1
      firstWeekStart = firstDay -
        CopticWeekdays.weekdayNumber (copticWeekdayFromDays firstDay)
   in firstWeekStart + 7 * (weekNumberValue week - 1) +
      CopticWeekdays.weekdayNumber target

public export
isValidCopticWeekDate : WeekNumber -> CopticDayOfWeek -> Year -> Bool
isValidCopticWeekDate week target valueYear =
  yearValue valueYear > 1 ||
  isValidCopticDays (copticWeekDateDays week target valueYear)

||| Construct a Coptic Sunday-based week date under static validity evidence.
public export
copticFromWeekDate : (week : WeekNumber) -> (target : CopticDayOfWeek) ->
                     (valueYear : Year) ->
                     {auto 0 valid : So
                       (isValidCopticWeekDate week target valueYear)} ->
                     CalendarDate Coptic
copticFromWeekDate week target valueYear =
  MkCopticDate (copticWeekDateDays week target valueYear)

||| Validate a runtime Coptic Sunday-based week date.
public export
refineCopticWeekDate : WeekNumber -> CopticDayOfWeek -> Year ->
                       Either CopticDateError (CalendarDate Coptic)
refineCopticWeekDate week target valueYear =
  case choose (isValidCopticWeekDate week target valueYear) of
    Left valid => Right (copticFromWeekDate week target valueYear @{valid})
    Right _ => Left (InvalidCopticWeekDate week target valueYear)
