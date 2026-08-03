module IotaTime.Calendar.Islamic

import IotaTime.Calendar
import IotaTime.Period
import Data.So

%default total

public export
data IslamicLeapPattern = Base15 | Base16 | Indian | HabashAlHasib

public export
interface KnownIslamicLeapPattern (pattern : IslamicLeapPattern) where
  leapCycleYears : List Integer

public export
KnownIslamicLeapPattern Base15 where
  leapCycleYears = [2, 5, 7, 10, 13, 15, 18, 21, 24, 26, 29]

public export
KnownIslamicLeapPattern Base16 where
  leapCycleYears = [2, 5, 7, 10, 13, 16, 18, 21, 24, 26, 29]

public export
KnownIslamicLeapPattern Indian where
  leapCycleYears = [2, 5, 8, 10, 13, 16, 19, 21, 24, 27, 29]

public export
KnownIslamicLeapPattern HabashAlHasib where
  leapCycleYears = [2, 5, 8, 11, 13, 16, 19, 21, 24, 27, 0]

public export
data Islamic : IslamicLeapPattern -> Type where
  IslamicCalendar : Islamic pattern

public export
IslamicBase15 : Type
IslamicBase15 = Islamic Base15

public export
IslamicBase16 : Type
IslamicBase16 = Islamic Base16

public export
IslamicIndian : Type
IslamicIndian = Islamic Indian

public export
IslamicHabashAlHasib : Type
IslamicHabashAlHasib = Islamic HabashAlHasib

public export
IslamicBcl : Type
IslamicBcl = IslamicBase16

namespace IslamicMonths
  public export
  data IslamicMonth
    = Muharram | Safar | RabiAlAwwal | RabiAlThani
    | JumadaAlAwwal | JumadaAlThani | Rajab | Shaban
    | Ramadan | Shawwal | DhulQadah | DhulHijjah

  public export
  monthNumber : IslamicMonth -> Integer
  monthNumber Muharram = 1
  monthNumber Safar = 2
  monthNumber RabiAlAwwal = 3
  monthNumber RabiAlThani = 4
  monthNumber JumadaAlAwwal = 5
  monthNumber JumadaAlThani = 6
  monthNumber Rajab = 7
  monthNumber Shaban = 8
  monthNumber Ramadan = 9
  monthNumber Shawwal = 10
  monthNumber DhulQadah = 11
  monthNumber DhulHijjah = 12

  public export
  Eq IslamicMonth where
    left == right = monthNumber left == monthNumber right

  public export
  Ord IslamicMonth where
    compare left right = compare (monthNumber left) (monthNumber right)

  public export
  Show IslamicMonth where
    show Muharram = "Muharram"
    show Safar = "Safar"
    show RabiAlAwwal = "RabiAlAwwal"
    show RabiAlThani = "RabiAlThani"
    show JumadaAlAwwal = "JumadaAlAwwal"
    show JumadaAlThani = "JumadaAlThani"
    show Rajab = "Rajab"
    show Shaban = "Shaban"
    show Ramadan = "Ramadan"
    show Shawwal = "Shawwal"
    show DhulQadah = "DhulQadah"
    show DhulHijjah = "DhulHijjah"

namespace IslamicWeekdays
  public export
  data IslamicDayOfWeek
    = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

  public export
  weekdayNumber : IslamicDayOfWeek -> Integer
  weekdayNumber Sunday = 0
  weekdayNumber Monday = 1
  weekdayNumber Tuesday = 2
  weekdayNumber Wednesday = 3
  weekdayNumber Thursday = 4
  weekdayNumber Friday = 5
  weekdayNumber Saturday = 6

  public export
  Eq IslamicDayOfWeek where
    left == right = weekdayNumber left == weekdayNumber right

  public export
  Ord IslamicDayOfWeek where
    compare left right = compare (weekdayNumber left) (weekdayNumber right)

  public export
  Show IslamicDayOfWeek where
    show Sunday = "Sunday"
    show Monday = "Monday"
    show Tuesday = "Tuesday"
    show Wednesday = "Wednesday"
    show Thursday = "Thursday"
    show Friday = "Friday"
    show Saturday = "Saturday"

monthFromNumber : Integer -> IslamicMonth
monthFromNumber 1 = IslamicMonths.Muharram
monthFromNumber 2 = IslamicMonths.Safar
monthFromNumber 3 = IslamicMonths.RabiAlAwwal
monthFromNumber 4 = IslamicMonths.RabiAlThani
monthFromNumber 5 = IslamicMonths.JumadaAlAwwal
monthFromNumber 6 = IslamicMonths.JumadaAlThani
monthFromNumber 7 = IslamicMonths.Rajab
monthFromNumber 8 = IslamicMonths.Shaban
monthFromNumber 9 = IslamicMonths.Ramadan
monthFromNumber 10 = IslamicMonths.Shawwal
monthFromNumber 11 = IslamicMonths.DhulQadah
monthFromNumber _ = IslamicMonths.DhulHijjah

public export
islamicWeekdayFromDays : Integer -> IslamicDayOfWeek
islamicWeekdayFromDays value = case (value + 3) `mod` 7 of
  0 => IslamicWeekdays.Sunday
  1 => IslamicWeekdays.Monday
  2 => IslamicWeekdays.Tuesday
  3 => IslamicWeekdays.Wednesday
  4 => IslamicWeekdays.Thursday
  5 => IslamicWeekdays.Friday
  _ => IslamicWeekdays.Saturday

export
record IslamicDate (pattern : IslamicLeapPattern) where
  constructor MkIslamicDate
  daysSinceEpoch : Integer

export
Eq (IslamicDate pattern) where
  left == right = left.daysSinceEpoch == right.daysSinceEpoch

export
Ord (IslamicDate pattern) where
  compare left right = compare left.daysSinceEpoch right.daysSinceEpoch

public export
islamicEpoch : Integer
islamicEpoch = -503166

public export
isIslamicLeapYear : {pattern : IslamicLeapPattern} ->
                    KnownIslamicLeapPattern pattern => Year -> Bool
isIslamicLeapYear value =
  let cycleYear = yearValue value `mod` 30
   in elem cycleYear (leapCycleYears {pattern})

countCycleLeaps : Integer -> List Integer -> Integer
countCycleLeaps _ [] = 0
countCycleLeaps years (position :: rest) =
  (if position > 0 && position <= years then 1 else 0) +
  countCycleLeaps years rest

leapsBeforeIslamicYear : {pattern : IslamicLeapPattern} ->
                         KnownIslamicLeapPattern pattern => Year -> Integer
leapsBeforeIslamicYear value =
  let priorYears = yearValue value - 1
      cycles = priorYears `div` 30
      yearsInCycle = priorYears `mod` 30
   in cycles * 11 + countCycleLeaps yearsInCycle (leapCycleYears {pattern})

public export
maxIslamicDaysInMonth : {pattern : IslamicLeapPattern} ->
                        KnownIslamicLeapPattern pattern =>
                        IslamicMonth -> Year -> DayOfMonth
maxIslamicDaysInMonth IslamicMonths.DhulHijjah value =
  if isIslamicLeapYear {pattern} value then 30 else 29
maxIslamicDaysInMonth valueMonth _ =
  if IslamicMonths.monthNumber valueMonth `mod` 2 == 1 then 30 else 29

public export
isValidIslamicDate : {pattern : IslamicLeapPattern} ->
                     KnownIslamicLeapPattern pattern =>
                     DayOfMonth -> IslamicMonth -> Year -> Bool
isValidIslamicDate valueDay valueMonth valueYear =
  let dayNumber = dayOfMonthValue valueDay
      maxDay = dayOfMonthValue
        (maxIslamicDaysInMonth {pattern} valueMonth valueYear)
   in dayNumber >= 1 && dayNumber <= maxDay && yearValue valueYear >= 1

monthOffset : IslamicMonth -> Integer
monthOffset value = ((IslamicMonths.monthNumber value - 1) * 59 + 1) `div` 2

public export
islamicDaysFromCivil : {pattern : IslamicLeapPattern} ->
                       KnownIslamicLeapPattern pattern =>
                       Year -> IslamicMonth -> DayOfMonth -> Integer
islamicDaysFromCivil valueYear valueMonth valueDay =
  islamicEpoch + (yearValue valueYear - 1) * 354 +
    leapsBeforeIslamicYear {pattern} valueYear + monthOffset valueMonth +
    dayOfMonthValue valueDay - 1

islamicYearLength : {pattern : IslamicLeapPattern} ->
                    KnownIslamicLeapPattern pattern => Integer -> Integer
islamicYearLength cycleYear =
  if elem (cycleYear `mod` 30) (leapCycleYears {pattern}) then 355 else 354

findIslamicYear : {pattern : IslamicLeapPattern} ->
                  KnownIslamicLeapPattern pattern =>
      Nat -> Integer -> Integer -> (Integer, Integer)
findIslamicYear Z cycleYear remaining = (cycleYear, remaining)
findIslamicYear (S fuel) cycleYear remaining =
  let length = islamicYearLength {pattern} (cycleYear + 1)
   in if remaining < length
        then (cycleYear, remaining)
  else findIslamicYear {pattern} fuel (cycleYear + 1) (remaining - length)

islamicCivilFromDays : {pattern : IslamicLeapPattern} ->
                       KnownIslamicLeapPattern pattern =>
                       Integer -> (Year, IslamicMonth, DayOfMonth)
islamicCivilFromDays value =
  let relative = value - islamicEpoch
      cycles = relative `div` 10631
      remaining = relative `mod` 10631
      (yearInCycle, dayOfYear) = findIslamicYear {pattern} 30 0 remaining
      yearNumber = cycles * 30 + yearInCycle + 1
      monthNumber = if dayOfYear == 354 then 12 else dayOfYear * 2 `div` 59 + 1
      offset = ((monthNumber - 1) * 59 + 1) `div` 2
      dayNumber = dayOfYear - offset + 1
   in (yearFromInteger yearNumber, monthFromNumber monthNumber,
       dayOfMonthFromInteger dayNumber)

public export
isValidIslamicDays : Integer -> Bool
isValidIslamicDays value = value >= islamicEpoch

public export
{pattern : IslamicLeapPattern} -> KnownIslamicLeapPattern pattern =>
  HasCalendarDate (IslamicDate pattern) where
  calendarDays = daysSinceEpoch
  acceptsCalendarDays = isValidIslamicDays
  calendarDateFromDays days = MkIslamicDate days
  calendarDateName = "Islamic"

makeIslamicDate : {pattern : IslamicLeapPattern} ->
                  KnownIslamicLeapPattern pattern => Integer -> IslamicDate pattern
makeIslamicDate days = MkIslamicDate days

clampToIslamic : Integer -> Integer
clampToIslamic = max islamicEpoch

shiftIslamicDays : {pattern : IslamicLeapPattern} ->
                   KnownIslamicLeapPattern pattern =>
                   Integer -> IslamicDate pattern -> IslamicDate pattern
shiftIslamicDays amount date =
  makeIslamicDate {pattern} (clampToIslamic (date.daysSinceEpoch + amount))

shiftIslamicMonths : {pattern : IslamicLeapPattern} ->
                     KnownIslamicLeapPattern pattern =>
                     Integer -> IslamicDate pattern -> IslamicDate pattern
shiftIslamicMonths amount date =
  let (valueYear, valueMonth, valueDay) =
        islamicCivilFromDays {pattern} date.daysSinceEpoch
      monthOrdinal = IslamicMonths.monthNumber valueMonth - 1 + amount
      targetYear = yearFromInteger
        (yearValue valueYear + monthOrdinal `div` 12)
      targetMonth = monthFromNumber (monthOrdinal `mod` 12 + 1)
      targetDay = min valueDay
        (maxIslamicDaysInMonth {pattern} targetMonth targetYear)
   in makeIslamicDate {pattern}
        (clampToIslamic
          (islamicDaysFromCivil {pattern} targetYear targetMonth targetDay))

shiftIslamicYears : {pattern : IslamicLeapPattern} ->
                    KnownIslamicLeapPattern pattern =>
                    Integer -> IslamicDate pattern -> IslamicDate pattern
shiftIslamicYears amount date =
  let (valueYear, valueMonth, valueDay) =
        islamicCivilFromDays {pattern} date.daysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + amount)
      targetDay = min valueDay
        (maxIslamicDaysInMonth {pattern} valueMonth targetYear)
   in makeIslamicDate {pattern}
        (clampToIslamic
          (islamicDaysFromCivil {pattern} targetYear valueMonth targetDay))

applyIslamicPeriod : {pattern : IslamicLeapPattern} ->
                     KnownIslamicLeapPattern pattern =>
                     Period target -> IslamicDate pattern -> IslamicDate pattern
applyIslamicPeriod period =
    shiftIslamicDays {pattern} (periodDays period)
  . shiftIslamicDays {pattern} (7 * periodWeeks period)
  . shiftIslamicMonths {pattern} (periodMonths period)
  . shiftIslamicYears {pattern} (periodYears period)

islamicDayOfWeek : IslamicDate pattern -> IslamicDayOfWeek
islamicDayOfWeek date = islamicWeekdayFromDays date.daysSinceEpoch

nextIslamic : {pattern : IslamicLeapPattern} ->
              KnownIslamicLeapPattern pattern =>
              Integer -> IslamicDayOfWeek -> IslamicDate pattern -> IslamicDate pattern
nextIslamic count target date =
  let current = IslamicWeekdays.weekdayNumber (islamicDayOfWeek date)
      wanted = IslamicWeekdays.weekdayNumber target
      weeks = if wanted > current then count - 1 else count
   in makeIslamicDate {pattern}
        (clampToIslamic (date.daysSinceEpoch + 7 * weeks + wanted - current))

previousIslamic : {pattern : IslamicLeapPattern} ->
                  KnownIslamicLeapPattern pattern =>
                  Integer -> IslamicDayOfWeek -> IslamicDate pattern -> IslamicDate pattern
previousIslamic count target date =
  let current = IslamicWeekdays.weekdayNumber (islamicDayOfWeek date)
      wanted = IslamicWeekdays.weekdayNumber target
      weeks = if wanted < current then count - 1 else count
   in makeIslamicDate {pattern}
        (clampToIslamic (date.daysSinceEpoch - (7 * weeks + current - wanted)))

public export
{pattern : IslamicLeapPattern} -> KnownIslamicLeapPattern pattern =>
  Calendar (Islamic pattern) where
  DateRep = IslamicDate pattern
  MonthRep _ = IslamicMonth
  WeekdayRep = IslamicDayOfWeek

  isValidDays = isValidIslamicDays
  fromDays days = makeIslamicDate {pattern} days
  toDays date = date.daysSinceEpoch
  calendarName = "Islamic"

  year' date = let (value, _, _) =
                    islamicCivilFromDays {pattern} date.daysSinceEpoch in value
  toYmd date = let (_, valueMonth, valueDay) =
                    islamicCivilFromDays {pattern} date.daysSinceEpoch
                in (valueMonth, valueDay)
  day' date = let (_, _, value) =
                   islamicCivilFromDays {pattern} date.daysSinceEpoch in value
  month' date = let (_, value, _) =
                     islamicCivilFromDays {pattern} date.daysSinceEpoch in value

  applyCalendarPeriod' = applyIslamicPeriod {pattern}
  shiftCalendarDays' = shiftIslamicDays {pattern}

  dayOfWeek = islamicDayOfWeek
  next = nextIslamic {pattern}
  previous = previousIslamic {pattern}

public export
{pattern : IslamicLeapPattern} -> HasCalendar (IslamicDate pattern) where
  calendarCapability = ()

public export
{pattern : IslamicLeapPattern} -> KnownIslamicLeapPattern pattern =>
  ApplyPeriod (IslamicDate pattern) where
  applyPeriod = applyIslamicPeriod {pattern}

public export
islamicDate' : {pattern : IslamicLeapPattern} ->
               {auto known : KnownIslamicLeapPattern pattern} ->
               (valueDay : DayOfMonth) -> (valueMonth : IslamicMonth) ->
               (valueYear : Year) ->
               {auto 0 valid : So
                 (isValidIslamicDate {pattern} valueDay valueMonth valueYear)} ->
               CalendarDate (Islamic pattern)
islamicDate' valueDay valueMonth valueYear =
  makeIslamicDate {pattern}
    (islamicDaysFromCivil {pattern} valueYear valueMonth valueDay)

public export
islamicDate : (valueDay : DayOfMonth) -> (valueMonth : IslamicMonth) ->
              (valueYear : Year) ->
              {auto 0 valid : So
                (isValidIslamicDate {pattern = Base16}
                  valueDay valueMonth valueYear)} ->
              CalendarDate IslamicBcl
islamicDate = islamicDate' {pattern = Base16}

public export
data IslamicDateError
  = InvalidIslamicDate DayOfMonth IslamicMonth Year
  | InvalidIslamicDayCount Integer
  | InvalidIslamicNthDay DayNth IslamicDayOfWeek IslamicMonth Year
  | InvalidIslamicWeekDate WeekNumber IslamicDayOfWeek Year

public export
refineIslamicDate' : {pattern : IslamicLeapPattern} ->
                     {auto known : KnownIslamicLeapPattern pattern} ->
                     DayOfMonth -> IslamicMonth -> Year ->
                     Either IslamicDateError (CalendarDate (Islamic pattern))
refineIslamicDate' @{known} valueDay valueMonth valueYear =
  case choose (isValidIslamicDate {pattern} valueDay valueMonth valueYear) of
    Left valid => Right
      (islamicDate' {pattern} @{known} valueDay valueMonth valueYear @{valid})
    Right _ => Left (InvalidIslamicDate valueDay valueMonth valueYear)

public export
refineIslamicDate : DayOfMonth -> IslamicMonth -> Year ->
                    Either IslamicDateError (CalendarDate IslamicBcl)
refineIslamicDate = refineIslamicDate' {pattern = Base16}

public export
islamicFromDays' : {pattern : IslamicLeapPattern} ->
                   {auto known : KnownIslamicLeapPattern pattern} ->
                   (days : Integer) ->
                   {auto 0 valid : So (isValidIslamicDays days)} ->
                   CalendarDate (Islamic pattern)
islamicFromDays' days = makeIslamicDate {pattern} days

public export
islamicFromDays : (days : Integer) ->
                  {auto 0 valid : So (isValidIslamicDays days)} ->
                  CalendarDate IslamicBcl
islamicFromDays = islamicFromDays' {pattern = Base16}

public export
refineIslamicDays' : {pattern : IslamicLeapPattern} ->
                     {auto known : KnownIslamicLeapPattern pattern} ->
                     Integer -> Either IslamicDateError
                       (CalendarDate (Islamic pattern))
refineIslamicDays' @{known} days = case choose (isValidIslamicDays days) of
  Left valid => Right (islamicFromDays' {pattern} @{known} days @{valid})
  Right _ => Left (InvalidIslamicDayCount days)

public export
refineIslamicDays : Integer -> Either IslamicDateError
                      (CalendarDate IslamicBcl)
refineIslamicDays = refineIslamicDays' {pattern = Base16}

public export
nthIslamicDayOfMonth : {pattern : IslamicLeapPattern} ->
                       KnownIslamicLeapPattern pattern =>
                       DayNth -> IslamicDayOfWeek -> IslamicMonth -> Year ->
                       DayOfMonth
nthIslamicDayOfMonth nth target valueMonth valueYear =
  let monthLength = maxIslamicDaysInMonth {pattern} valueMonth valueYear
      firstOffset = (IslamicWeekdays.weekdayNumber target -
        IslamicWeekdays.weekdayNumber (islamicWeekdayFromDays
          (islamicDaysFromCivil {pattern} valueYear valueMonth 1))) `mod` 7
      lastOffset = (IslamicWeekdays.weekdayNumber (islamicWeekdayFromDays
        (islamicDaysFromCivil {pattern} valueYear valueMonth monthLength)) -
        IslamicWeekdays.weekdayNumber target) `mod` 7
      dayNumber = case nth of
        First => 1 + firstOffset
        Second => 8 + firstOffset
        Third => 15 + firstOffset
        Fourth => 22 + firstOffset
        Fifth => 29 + firstOffset
        Last => dayOfMonthValue monthLength - lastOffset
   in dayOfMonthFromInteger dayNumber

public export
isValidIslamicNthDay : {pattern : IslamicLeapPattern} ->
                       KnownIslamicLeapPattern pattern =>
                       DayNth -> IslamicDayOfWeek -> IslamicMonth -> Year -> Bool
isValidIslamicNthDay nth target valueMonth valueYear =
  yearValue valueYear >= 1 && case nth of
    Fifth => nthIslamicDayOfMonth {pattern} nth target valueMonth valueYear <=
      maxIslamicDaysInMonth {pattern} valueMonth valueYear
    _ => True

public export
islamicFromNthDay' : {pattern : IslamicLeapPattern} ->
                     {auto known : KnownIslamicLeapPattern pattern} ->
                     (nth : DayNth) -> (target : IslamicDayOfWeek) ->
                     (valueMonth : IslamicMonth) -> (valueYear : Year) ->
                     {auto 0 valid : So
                       (isValidIslamicNthDay {pattern}
                         nth target valueMonth valueYear)} ->
                     CalendarDate (Islamic pattern)
islamicFromNthDay' nth target valueMonth valueYear =
  makeIslamicDate {pattern} (islamicDaysFromCivil {pattern} valueYear valueMonth
    (nthIslamicDayOfMonth {pattern} nth target valueMonth valueYear))

public export
islamicFromNthDay : (nth : DayNth) -> (target : IslamicDayOfWeek) ->
                    (valueMonth : IslamicMonth) -> (valueYear : Year) ->
                    {auto 0 valid : So
                      (isValidIslamicNthDay {pattern = Base16}
                        nth target valueMonth valueYear)} ->
                    CalendarDate IslamicBcl
islamicFromNthDay = islamicFromNthDay' {pattern = Base16}

public export
refineIslamicNthDay' : {pattern : IslamicLeapPattern} ->
                       {auto known : KnownIslamicLeapPattern pattern} ->
                       DayNth -> IslamicDayOfWeek -> IslamicMonth -> Year ->
                       Either IslamicDateError (CalendarDate (Islamic pattern))
refineIslamicNthDay' @{known} nth target valueMonth valueYear =
  case choose (isValidIslamicNthDay {pattern}
    nth target valueMonth valueYear) of
      Left valid => Right
        (islamicFromNthDay' {pattern}
          @{known} nth target valueMonth valueYear @{valid})
      Right _ => Left (InvalidIslamicNthDay nth target valueMonth valueYear)

public export
refineIslamicNthDay : DayNth -> IslamicDayOfWeek -> IslamicMonth -> Year ->
                      Either IslamicDateError (CalendarDate IslamicBcl)
refineIslamicNthDay = refineIslamicNthDay' {pattern = Base16}

public export
islamicWeekDateDays : {pattern : IslamicLeapPattern} ->
                      KnownIslamicLeapPattern pattern =>
                      WeekNumber -> IslamicDayOfWeek -> Year -> Integer
islamicWeekDateDays week target valueYear =
  let firstDay = islamicDaysFromCivil {pattern}
        valueYear IslamicMonths.Muharram 1
      firstWeekStart = firstDay -
        ((IslamicWeekdays.weekdayNumber (islamicWeekdayFromDays firstDay) - 6)
          `mod` 7)
      targetOffset = (IslamicWeekdays.weekdayNumber target - 6) `mod` 7
   in firstWeekStart + 7 * (weekNumberValue week - 1) + targetOffset

public export
isValidIslamicWeekDate : {pattern : IslamicLeapPattern} ->
                         KnownIslamicLeapPattern pattern =>
                         WeekNumber -> IslamicDayOfWeek -> Year -> Bool
isValidIslamicWeekDate week target valueYear =
  yearValue valueYear > 1 ||
  isValidIslamicDays (islamicWeekDateDays {pattern} week target valueYear)

public export
islamicFromWeekDate' : {pattern : IslamicLeapPattern} ->
                       {auto known : KnownIslamicLeapPattern pattern} ->
                       (week : WeekNumber) -> (target : IslamicDayOfWeek) ->
                       (valueYear : Year) ->
                       {auto 0 valid : So
                         (isValidIslamicWeekDate {pattern}
                           week target valueYear)} ->
                       CalendarDate (Islamic pattern)
islamicFromWeekDate' week target valueYear =
  makeIslamicDate {pattern}
    (islamicWeekDateDays {pattern} week target valueYear)

public export
islamicFromWeekDate : (week : WeekNumber) ->
                      (target : IslamicDayOfWeek) -> (valueYear : Year) ->
                      {auto 0 valid : So
                        (isValidIslamicWeekDate {pattern = Base16}
                          week target valueYear)} ->
                      CalendarDate IslamicBcl
islamicFromWeekDate = islamicFromWeekDate' {pattern = Base16}

public export
refineIslamicWeekDate' : {pattern : IslamicLeapPattern} ->
                         {auto known : KnownIslamicLeapPattern pattern} ->
                         WeekNumber -> IslamicDayOfWeek -> Year ->
                         Either IslamicDateError
                           (CalendarDate (Islamic pattern))
refineIslamicWeekDate' @{known} week target valueYear =
  case choose (isValidIslamicWeekDate {pattern} week target valueYear) of
    Left valid => Right
      (islamicFromWeekDate' {pattern} @{known} week target valueYear @{valid})
    Right _ => Left (InvalidIslamicWeekDate week target valueYear)

public export
refineIslamicWeekDate : WeekNumber -> IslamicDayOfWeek -> Year ->
                        Either IslamicDateError (CalendarDate IslamicBcl)
refineIslamicWeekDate = refineIslamicWeekDate' {pattern = Base16}
