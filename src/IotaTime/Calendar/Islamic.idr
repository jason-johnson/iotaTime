module IotaTime.Calendar.Islamic

import IotaTime.Calendar
import IotaTime.Period
import Data.So
import Derive.Prelude

%language ElabReflection

%default total

||| The supported tabular Islamic 30-year leap-cycle assignments.
public export
data IslamicLeapPattern = Base15 | Base16 | Indian | HabashAlHasib

||| Evidence and leap-year positions for one Islamic leap pattern.
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

||| The two conventional epochs used by tabular Islamic calendars.
public export
data IslamicEpoch = Astronomical | Civil

||| Evidence for the first timeline day represented by an Islamic epoch.
public export
interface KnownIslamicEpoch (epoch : IslamicEpoch) where
  epochDay : Integer
  dateConstructorName : String

public export
KnownIslamicEpoch Astronomical where
  epochDay = -503166
  dateConstructorName = "calendarDate'"

public export
KnownIslamicEpoch Civil where
  epochDay = -503165
  dateConstructorName = "civilCalendarDate'"

||| A tabular Islamic calendar indexed by its epoch and leap-cycle pattern.
public export
data IslamicByEpoch : IslamicEpoch -> IslamicLeapPattern -> Type where
  IslamicCalendar : IslamicByEpoch epoch pattern

||| The astronomical-epoch calendar retained by the original iotaTime API.
public export
Islamic : IslamicLeapPattern -> Type
Islamic pattern = IslamicByEpoch Astronomical pattern

||| A civil-epoch tabular Islamic calendar.
public export
CivilIslamic : IslamicLeapPattern -> Type
CivilIslamic pattern = IslamicByEpoch Civil pattern

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

public export
CivilIslamicBase15 : Type
CivilIslamicBase15 = CivilIslamic Base15

public export
CivilIslamicBase16 : Type
CivilIslamicBase16 = CivilIslamic Base16

public export
CivilIslamicIndian : Type
CivilIslamicIndian = CivilIslamic Indian

public export
CivilIslamicHabashAlHasib : Type
CivilIslamicHabashAlHasib = CivilIslamic HabashAlHasib

public export
CivilIslamicBcl : Type
CivilIslamicBcl = CivilIslamicBase16

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

  %runElab derive `{IslamicMonth} [Show]

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

  %runElab derive `{IslamicDayOfWeek} [Show]

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
record IslamicDate (epoch : IslamicEpoch) (pattern : IslamicLeapPattern) where
  constructor MkIslamicDate
  daysSinceEpoch : Integer

public export
Eq (IslamicDate epoch pattern) where
  left == right = left.daysSinceEpoch == right.daysSinceEpoch

public export
Ord (IslamicDate epoch pattern) where
  compare left right = compare left.daysSinceEpoch right.daysSinceEpoch

public export
isLeapYear : {pattern : IslamicLeapPattern} ->
                    KnownIslamicLeapPattern pattern => Year -> Bool
isLeapYear value =
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
maxDaysInMonth : {pattern : IslamicLeapPattern} ->
                        KnownIslamicLeapPattern pattern =>
                        IslamicMonth -> Year -> DayOfMonth
maxDaysInMonth IslamicMonths.DhulHijjah value =
  if isLeapYear {pattern} value then 30 else 29
maxDaysInMonth valueMonth _ =
  if IslamicMonths.monthNumber valueMonth `mod` 2 == 1 then 30 else 29

public export
isValidDate : {pattern : IslamicLeapPattern} ->
                     KnownIslamicLeapPattern pattern =>
                     DayOfMonth -> IslamicMonth -> Year -> Bool
isValidDate valueDay valueMonth valueYear =
  let dayNumber = dayOfMonthValue valueDay
      maxDay = dayOfMonthValue
        (maxDaysInMonth {pattern} valueMonth valueYear)
   in dayNumber >= 1 && dayNumber <= maxDay && yearValue valueYear >= 1

monthOffset : IslamicMonth -> Integer
monthOffset value = ((IslamicMonths.monthNumber value - 1) * 59 + 1) `div` 2

islamicDaysFromCivil : {epoch : IslamicEpoch} ->
                       {pattern : IslamicLeapPattern} ->
                       KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
                       Year -> IslamicMonth -> DayOfMonth -> Integer
islamicDaysFromCivil valueYear valueMonth valueDay =
  epochDay {epoch} + (yearValue valueYear - 1) * 354 +
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

islamicCivilFromDays : {epoch : IslamicEpoch} ->
                       {pattern : IslamicLeapPattern} ->
                       KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
                       Integer -> (Year, IslamicMonth, DayOfMonth)
islamicCivilFromDays value =
  let relative = value - epochDay {epoch}
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
{epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
  KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
  HasCalendarDate (IslamicDate epoch pattern) where
  calendarDays = daysSinceEpoch
  acceptsCalendarDays = (>= epochDay {epoch})
  calendarDateFromDays days = MkIslamicDate days
  calendarDateName = "Islamic"

makeIslamicDate : {epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
                  KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
                  Integer -> IslamicDate epoch pattern
makeIslamicDate days = MkIslamicDate days

clampToIslamic : {epoch : IslamicEpoch} -> KnownIslamicEpoch epoch =>
                 Integer -> Integer
clampToIslamic = max (epochDay {epoch})

shiftIslamicDays : {epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
                   KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
                   Integer -> IslamicDate epoch pattern -> IslamicDate epoch pattern
shiftIslamicDays amount date =
  makeIslamicDate {epoch} {pattern}
    (clampToIslamic {epoch} (date.daysSinceEpoch + amount))

shiftIslamicMonths : {epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
                     KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
                     Integer -> IslamicDate epoch pattern -> IslamicDate epoch pattern
shiftIslamicMonths amount date =
  let (valueYear, valueMonth, valueDay) =
      islamicCivilFromDays {epoch} {pattern} date.daysSinceEpoch
      monthOrdinal = IslamicMonths.monthNumber valueMonth - 1 + amount
      targetYear = yearFromInteger
        (yearValue valueYear + monthOrdinal `div` 12)
      targetMonth = monthFromNumber (monthOrdinal `mod` 12 + 1)
      targetDay = min valueDay
        (maxDaysInMonth {pattern} targetMonth targetYear)
   in makeIslamicDate {epoch} {pattern}
        (clampToIslamic {epoch}
          (islamicDaysFromCivil {epoch} {pattern}
            targetYear targetMonth targetDay))

shiftIslamicYears : {epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
                    KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
                    Integer -> IslamicDate epoch pattern -> IslamicDate epoch pattern
shiftIslamicYears amount date =
  let (valueYear, valueMonth, valueDay) =
      islamicCivilFromDays {epoch} {pattern} date.daysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + amount)
      targetDay = min valueDay
        (maxDaysInMonth {pattern} valueMonth targetYear)
   in makeIslamicDate {epoch} {pattern}
        (clampToIslamic {epoch}
          (islamicDaysFromCivil {epoch} {pattern}
            targetYear valueMonth targetDay))

applyIslamicPeriod : {epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
                     KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
                     Period target -> IslamicDate epoch pattern -> IslamicDate epoch pattern
applyIslamicPeriod period =
    shiftIslamicDays {epoch} {pattern} (periodDays period)
  . shiftIslamicDays {epoch} {pattern} (7 * periodWeeks period)
  . shiftIslamicMonths {epoch} {pattern} (periodMonths period)
  . shiftIslamicYears {epoch} {pattern} (periodYears period)

islamicDayOfWeek : IslamicDate epoch pattern -> IslamicDayOfWeek
islamicDayOfWeek date = islamicWeekdayFromDays date.daysSinceEpoch

nextIslamic : {epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
              KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
              Integer -> IslamicDayOfWeek -> IslamicDate epoch pattern ->
              IslamicDate epoch pattern
nextIslamic count target date =
  let current = IslamicWeekdays.weekdayNumber (islamicDayOfWeek date)
      wanted = IslamicWeekdays.weekdayNumber target
      weeks = if wanted > current then count - 1 else count
     in makeIslamicDate {epoch} {pattern}
       (clampToIslamic {epoch}
      (date.daysSinceEpoch + 7 * weeks + wanted - current))

previousIslamic : {epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
                  KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
                  Integer -> IslamicDayOfWeek -> IslamicDate epoch pattern ->
                  IslamicDate epoch pattern
previousIslamic count target date =
  let current = IslamicWeekdays.weekdayNumber (islamicDayOfWeek date)
      wanted = IslamicWeekdays.weekdayNumber target
      weeks = if wanted < current then count - 1 else count
     in makeIslamicDate {epoch} {pattern}
       (clampToIslamic {epoch}
      (date.daysSinceEpoch - (7 * weeks + current - wanted)))

public export
{epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
  KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
  Calendar (IslamicByEpoch epoch pattern) where
  DateRep = IslamicDate epoch pattern
  MonthRep _ = IslamicMonth
  WeekdayRep = IslamicDayOfWeek

  isValidDays = (>= epochDay {epoch})
  fromDays days = makeIslamicDate {epoch} {pattern} days
  toDays date = date.daysSinceEpoch
  calendarName = "Islamic"

  year' date = let (value, _, _) =
                    islamicCivilFromDays {epoch} {pattern}
                      date.daysSinceEpoch in value
  toYmd date = let (_, valueMonth, valueDay) =
                    islamicCivilFromDays {epoch} {pattern} date.daysSinceEpoch
                in (valueMonth, valueDay)
  day' date = let (_, _, value) =
                   islamicCivilFromDays {epoch} {pattern}
                     date.daysSinceEpoch in value
  month' date = let (_, value, _) =
                     islamicCivilFromDays {epoch} {pattern}
                       date.daysSinceEpoch in value

  applyCalendarPeriod' = applyIslamicPeriod {epoch} {pattern}
  shiftCalendarDays' = shiftIslamicDays {epoch} {pattern}

  dayOfWeek = islamicDayOfWeek
  next = nextIslamic {epoch} {pattern}
  previous = previousIslamic {epoch} {pattern}

public export
{epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
  KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
  Show (IslamicDate epoch pattern) where
  show date = case islamicCivilFromDays {epoch} {pattern}
    date.daysSinceEpoch of
    (valueYear, valueMonth, valueDay) =>
      dateConstructorName {epoch} ++ " " ++ show valueDay ++ " " ++
      show valueMonth ++ " " ++ show valueYear

public export
{epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
  HasCalendar (IslamicDate epoch pattern) where
  calendarCapability = ()

public export
{epoch : IslamicEpoch} -> {pattern : IslamicLeapPattern} ->
  KnownIslamicEpoch epoch => KnownIslamicLeapPattern pattern =>
  ApplyPeriod (IslamicDate epoch pattern) where
  applyPeriod = applyIslamicPeriod {epoch} {pattern}

||| Construct a statically validated Islamic date for the selected leap pattern.
public export
calendarDate' : {pattern : IslamicLeapPattern} ->
               {auto known : KnownIslamicLeapPattern pattern} ->
               (valueDay : DayOfMonth) -> (valueMonth : IslamicMonth) ->
               (valueYear : Year) ->
               {auto 0 valid : So
                 (isValidDate {pattern} valueDay valueMonth valueYear)} ->
               CalendarDate (Islamic pattern)
calendarDate' valueDay valueMonth valueYear =
  makeIslamicDate {epoch = Astronomical} {pattern}
    (islamicDaysFromCivil {epoch = Astronomical} {pattern}
      valueYear valueMonth valueDay)

||| Construct a statically validated Base16/BCL Islamic date.
public export
calendarDate : (valueDay : DayOfMonth) -> (valueMonth : IslamicMonth) ->
              (valueYear : Year) ->
              {auto 0 valid : So
                (isValidDate {pattern = Base16}
                  valueDay valueMonth valueYear)} ->
              CalendarDate IslamicBcl
calendarDate = calendarDate' {pattern = Base16}

||| Failures produced while refining untrusted Islamic date data.
public export
data IslamicDateError
  = InvalidIslamicDate DayOfMonth IslamicMonth Year
  | InvalidIslamicDayCount Integer
  | InvalidIslamicNthDay DayNth IslamicDayOfWeek IslamicMonth Year
  | InvalidIslamicWeekDate WeekNumber IslamicDayOfWeek Year

||| Validate runtime date components for the selected Islamic leap pattern.
public export
refineDate' : {pattern : IslamicLeapPattern} ->
                     {auto known : KnownIslamicLeapPattern pattern} ->
                     DayOfMonth -> IslamicMonth -> Year ->
                     Either IslamicDateError (CalendarDate (Islamic pattern))
refineDate' @{known} valueDay valueMonth valueYear =
  case choose (isValidDate {pattern} valueDay valueMonth valueYear) of
    Left valid => Right
      (calendarDate' {pattern} @{known}
        valueDay valueMonth valueYear @{valid})
    Right _ => Left (InvalidIslamicDate valueDay valueMonth valueYear)

||| Validate runtime date components using the Base16/BCL leap pattern.
public export
refineDate : DayOfMonth -> IslamicMonth -> Year ->
                    Either IslamicDateError (CalendarDate IslamicBcl)
refineDate = refineDate' {pattern = Base16}

||| Construct a date in the selected Islamic pattern from a statically valid
||| calendar-relative day count.
public export
fromDays' : {pattern : IslamicLeapPattern} ->
                   {auto known : KnownIslamicLeapPattern pattern} ->
                   (days : Integer) ->
                   {auto 0 valid : So
                     (IotaTime.Calendar.isValidDays
                       {calendar = Islamic pattern} days)} ->
                   CalendarDate (Islamic pattern)
fromDays' days = makeIslamicDate {pattern} days

public export
fromDays : (days : Integer) ->
                  {auto 0 valid : So
                    (IotaTime.Calendar.isValidDays
                      {calendar = IslamicBcl} days)} ->
                  CalendarDate IslamicBcl
fromDays = fromDays' {pattern = Base16}

||| Validate a runtime day count for the selected Islamic leap pattern.
public export
refineDays' : {pattern : IslamicLeapPattern} ->
                     {auto known : KnownIslamicLeapPattern pattern} ->
                     Integer -> Either IslamicDateError
                       (CalendarDate (Islamic pattern))
refineDays' @{known} days = case choose
  (IotaTime.Calendar.isValidDays {calendar = Islamic pattern} days) of
  Left valid => Right (fromDays' {pattern} @{known} days @{valid})
  Right _ => Left (InvalidIslamicDayCount days)

public export
refineDays : Integer -> Either IslamicDateError
                      (CalendarDate IslamicBcl)
refineDays = refineDays' {pattern = Base16}

||| Construct a statically validated civil-epoch Islamic date for the selected
||| leap pattern.
public export
civilCalendarDate' : {pattern : IslamicLeapPattern} ->
                    {auto known : KnownIslamicLeapPattern pattern} ->
                    (valueDay : DayOfMonth) ->
                    (valueMonth : IslamicMonth) -> (valueYear : Year) ->
                    {auto 0 valid : So
                      (isValidDate {pattern}
                        valueDay valueMonth valueYear)} ->
                    CalendarDate (CivilIslamic pattern)
civilCalendarDate' valueDay valueMonth valueYear =
  makeIslamicDate {epoch = Civil} {pattern}
    (islamicDaysFromCivil {epoch = Civil} {pattern}
      valueYear valueMonth valueDay)

||| Construct a statically validated Base16 civil-epoch Islamic date.
public export
civilCalendarDate : (valueDay : DayOfMonth) ->
                   (valueMonth : IslamicMonth) -> (valueYear : Year) ->
                   {auto 0 valid : So
                     (isValidDate {pattern = Base16}
                       valueDay valueMonth valueYear)} ->
                   CalendarDate CivilIslamicBcl
civilCalendarDate = civilCalendarDate' {pattern = Base16}

||| Validate runtime date components for a selected civil-epoch leap pattern.
public export
refineCivilDate' : {pattern : IslamicLeapPattern} ->
                          {auto known : KnownIslamicLeapPattern pattern} ->
                          DayOfMonth -> IslamicMonth -> Year ->
                          Either IslamicDateError
                            (CalendarDate (CivilIslamic pattern))
refineCivilDate' @{known} valueDay valueMonth valueYear =
  case choose (isValidDate {pattern}
    valueDay valueMonth valueYear) of
      Left valid => Right (civilCalendarDate' {pattern} @{known}
        valueDay valueMonth valueYear @{valid})
      Right _ => Left (InvalidIslamicDate valueDay valueMonth valueYear)

||| Validate runtime civil-epoch date components using the Base16 pattern.
public export
refineCivilDate : DayOfMonth -> IslamicMonth -> Year ->
                         Either IslamicDateError
                           (CalendarDate CivilIslamicBcl)
refineCivilDate = refineCivilDate' {pattern = Base16}

||| Construct a civil-epoch date from a statically valid timeline day count.
public export
civilFromDays' : {pattern : IslamicLeapPattern} ->
                        {auto known : KnownIslamicLeapPattern pattern} ->
                        (days : Integer) ->
                        {auto 0 valid : So
                          (IotaTime.Calendar.isValidDays
                            {calendar = CivilIslamic pattern} days)} ->
                        CalendarDate (CivilIslamic pattern)
civilFromDays' days = makeIslamicDate {epoch = Civil} {pattern} days

public export
civilFromDays : (days : Integer) ->
                       {auto 0 valid : So
                         (IotaTime.Calendar.isValidDays
                           {calendar = CivilIslamicBcl} days)} ->
                       CalendarDate CivilIslamicBcl
civilFromDays = civilFromDays' {pattern = Base16}

||| Validate a runtime timeline day count for a selected civil leap pattern.
public export
refineCivilDays' : {pattern : IslamicLeapPattern} ->
                          {auto known : KnownIslamicLeapPattern pattern} ->
                          Integer -> Either IslamicDateError
                            (CalendarDate (CivilIslamic pattern))
refineCivilDays' @{known} days =
  case choose
    (IotaTime.Calendar.isValidDays
      {calendar = CivilIslamic pattern} days) of
    Left valid => Right
      (civilFromDays' {pattern} @{known} days @{valid})
    Right _ => Left (InvalidIslamicDayCount days)

public export
refineCivilDays : Integer -> Either IslamicDateError
                           (CalendarDate CivilIslamicBcl)
refineCivilDays = refineCivilDays' {pattern = Base16}

nthIslamicDayOfMonthFor : {epoch : IslamicEpoch} ->
                          {pattern : IslamicLeapPattern} ->
                          KnownIslamicEpoch epoch =>
                          KnownIslamicLeapPattern pattern =>
                          DayNth -> IslamicDayOfWeek -> IslamicMonth -> Year ->
                          DayOfMonth
nthIslamicDayOfMonthFor nth target valueMonth valueYear =
  let monthLength = maxDaysInMonth {pattern} valueMonth valueYear
      firstOffset = (IslamicWeekdays.weekdayNumber target -
        IslamicWeekdays.weekdayNumber (islamicWeekdayFromDays
          (islamicDaysFromCivil {epoch} {pattern}
            valueYear valueMonth 1))) `mod` daysPerWeek
      lastOffset = (IslamicWeekdays.weekdayNumber (islamicWeekdayFromDays
        (islamicDaysFromCivil {epoch} {pattern}
          valueYear valueMonth monthLength)) -
        IslamicWeekdays.weekdayNumber target) `mod` daysPerWeek
      dayNumber = nthWeekdayDayNumber nth (dayOfMonthValue monthLength)
        firstOffset lastOffset
   in dayOfMonthFromInteger dayNumber

public export
nthDayOfMonth : {pattern : IslamicLeapPattern} ->
                       KnownIslamicLeapPattern pattern =>
                       DayNth -> IslamicDayOfWeek -> IslamicMonth -> Year ->
                       DayOfMonth
nthDayOfMonth =
  nthIslamicDayOfMonthFor {epoch = Astronomical} {pattern}

public export
civilNthDayOfMonth : {pattern : IslamicLeapPattern} ->
                            KnownIslamicLeapPattern pattern =>
                            DayNth -> IslamicDayOfWeek -> IslamicMonth ->
                            Year -> DayOfMonth
civilNthDayOfMonth =
  nthIslamicDayOfMonthFor {epoch = Civil} {pattern}

public export
isValidNthDay : {pattern : IslamicLeapPattern} ->
                       KnownIslamicLeapPattern pattern =>
                       DayNth -> IslamicDayOfWeek -> IslamicMonth -> Year -> Bool
isValidNthDay nth target valueMonth valueYear =
  yearValue valueYear >= 1 && case nth of
    Fifth => nthDayOfMonth {pattern} nth target valueMonth valueYear <=
      maxDaysInMonth {pattern} valueMonth valueYear
    _ => True

public export
isValidCivilNthDay : {pattern : IslamicLeapPattern} ->
                            KnownIslamicLeapPattern pattern =>
                            DayNth -> IslamicDayOfWeek -> IslamicMonth ->
                            Year -> Bool
isValidCivilNthDay nth target valueMonth valueYear =
  yearValue valueYear >= 1 && case nth of
    Fifth => civilNthDayOfMonth {pattern}
      nth target valueMonth valueYear <=
        maxDaysInMonth {pattern} valueMonth valueYear
    _ => True

||| Construct the nth requested weekday in an Islamic month for the selected
||| leap pattern.
public export
fromNthDay' : {pattern : IslamicLeapPattern} ->
                     {auto known : KnownIslamicLeapPattern pattern} ->
                     (nth : DayNth) -> (target : IslamicDayOfWeek) ->
                     (valueMonth : IslamicMonth) -> (valueYear : Year) ->
                     {auto 0 valid : So
                       (isValidNthDay {pattern}
                         nth target valueMonth valueYear)} ->
                     CalendarDate (Islamic pattern)
fromNthDay' nth target valueMonth valueYear =
  makeIslamicDate {epoch = Astronomical} {pattern}
    (islamicDaysFromCivil {epoch = Astronomical} {pattern}
      valueYear valueMonth
      (nthDayOfMonth {pattern} nth target valueMonth valueYear))

public export
fromNthDay : (nth : DayNth) -> (target : IslamicDayOfWeek) ->
                    (valueMonth : IslamicMonth) -> (valueYear : Year) ->
                    {auto 0 valid : So
                      (isValidNthDay {pattern = Base16}
                        nth target valueMonth valueYear)} ->
                    CalendarDate IslamicBcl
fromNthDay = fromNthDay' {pattern = Base16}

||| Validate an nth-weekday request for the selected Islamic leap pattern.
public export
refineNthDay' : {pattern : IslamicLeapPattern} ->
                       {auto known : KnownIslamicLeapPattern pattern} ->
                       DayNth -> IslamicDayOfWeek -> IslamicMonth -> Year ->
                       Either IslamicDateError (CalendarDate (Islamic pattern))
refineNthDay' @{known} nth target valueMonth valueYear =
  case choose (isValidNthDay {pattern}
    nth target valueMonth valueYear) of
      Left valid => Right
        (fromNthDay' {pattern}
          @{known} nth target valueMonth valueYear @{valid})
      Right _ => Left (InvalidIslamicNthDay nth target valueMonth valueYear)

public export
refineNthDay : DayNth -> IslamicDayOfWeek -> IslamicMonth -> Year ->
                      Either IslamicDateError (CalendarDate IslamicBcl)
refineNthDay = refineNthDay' {pattern = Base16}

||| Construct the nth requested weekday in a civil-epoch Islamic month.
public export
civilFromNthDay' : {pattern : IslamicLeapPattern} ->
                          {auto known : KnownIslamicLeapPattern pattern} ->
                          (nth : DayNth) -> (target : IslamicDayOfWeek) ->
                          (valueMonth : IslamicMonth) -> (valueYear : Year) ->
                          {auto 0 valid : So
                            (isValidCivilNthDay {pattern}
                              nth target valueMonth valueYear)} ->
                          CalendarDate (CivilIslamic pattern)
civilFromNthDay' nth target valueMonth valueYear =
  makeIslamicDate {epoch = Civil} {pattern}
    (islamicDaysFromCivil {epoch = Civil} {pattern}
      valueYear valueMonth
      (civilNthDayOfMonth {pattern}
        nth target valueMonth valueYear))

public export
civilFromNthDay : (nth : DayNth) ->
                         (target : IslamicDayOfWeek) ->
                         (valueMonth : IslamicMonth) -> (valueYear : Year) ->
                         {auto 0 valid : So
                           (isValidCivilNthDay {pattern = Base16}
                             nth target valueMonth valueYear)} ->
                         CalendarDate CivilIslamicBcl
civilFromNthDay = civilFromNthDay' {pattern = Base16}

public export
refineCivilNthDay' : {pattern : IslamicLeapPattern} ->
                            {auto known : KnownIslamicLeapPattern pattern} ->
                            DayNth -> IslamicDayOfWeek -> IslamicMonth -> Year ->
                            Either IslamicDateError
                              (CalendarDate (CivilIslamic pattern))
refineCivilNthDay' @{known} nth target valueMonth valueYear =
  case choose (isValidCivilNthDay {pattern}
    nth target valueMonth valueYear) of
      Left valid => Right (civilFromNthDay' {pattern} @{known}
        nth target valueMonth valueYear @{valid})
      Right _ => Left
        (InvalidIslamicNthDay nth target valueMonth valueYear)

public export
refineCivilNthDay : DayNth -> IslamicDayOfWeek -> IslamicMonth ->
                           Year -> Either IslamicDateError
                             (CalendarDate CivilIslamicBcl)
refineCivilNthDay = refineCivilNthDay' {pattern = Base16}

islamicWeekDateDaysFor : {epoch : IslamicEpoch} ->
                         {pattern : IslamicLeapPattern} ->
                         KnownIslamicEpoch epoch =>
                         KnownIslamicLeapPattern pattern =>
                         WeekNumber -> IslamicDayOfWeek -> Year -> Integer
islamicWeekDateDaysFor week target valueYear =
  let firstDay = islamicDaysFromCivil {epoch} {pattern}
        valueYear IslamicMonths.Muharram 1
      firstWeekStart = firstDay -
        ((IslamicWeekdays.weekdayNumber (islamicWeekdayFromDays firstDay) - 6)
          `mod` 7)
      targetOffset = (IslamicWeekdays.weekdayNumber target - 6) `mod` 7
   in firstWeekStart + 7 * (weekNumberValue week - 1) + targetOffset

public export
weekDateDays : {pattern : IslamicLeapPattern} ->
                      KnownIslamicLeapPattern pattern =>
                      WeekNumber -> IslamicDayOfWeek -> Year -> Integer
weekDateDays =
  islamicWeekDateDaysFor {epoch = Astronomical} {pattern}

public export
civilWeekDateDays : {pattern : IslamicLeapPattern} ->
                           KnownIslamicLeapPattern pattern =>
                           WeekNumber -> IslamicDayOfWeek -> Year -> Integer
civilWeekDateDays =
  islamicWeekDateDaysFor {epoch = Civil} {pattern}

public export
isValidWeekDate : {pattern : IslamicLeapPattern} ->
                  KnownIslamicLeapPattern pattern =>
                  WeekNumber -> IslamicDayOfWeek -> Year -> Bool
isValidWeekDate week target valueYear =
  (yearValue valueYear > 1 && weekNumberValue week >= 0) ||
    IotaTime.Calendar.isValidDays {calendar = Islamic pattern}
      (weekDateDays {pattern} week target valueYear)

public export
isValidCivilWeekDate : {pattern : IslamicLeapPattern} ->
                       KnownIslamicLeapPattern pattern =>
                       WeekNumber -> IslamicDayOfWeek -> Year -> Bool
isValidCivilWeekDate week target valueYear =
  (yearValue valueYear > 1 && weekNumberValue week >= 0) ||
    IotaTime.Calendar.isValidDays {calendar = CivilIslamic pattern}
      (civilWeekDateDays {pattern} week target valueYear)

||| Construct a Saturday-based Islamic week date for the selected leap pattern.
public export
fromWeekDate' : {pattern : IslamicLeapPattern} ->
                {auto known : KnownIslamicLeapPattern pattern} ->
                (week : WeekNumber) -> (target : IslamicDayOfWeek) ->
                (valueYear : Year) ->
                {auto 0 valid : So
                  (isValidWeekDate {pattern} week target valueYear)} ->
                CalendarDate (Islamic pattern)
fromWeekDate' week target valueYear =
  makeIslamicDate {epoch = Astronomical} {pattern}
    (weekDateDays {pattern} week target valueYear)

public export
fromWeekDate : (week : WeekNumber) ->
               (target : IslamicDayOfWeek) -> (valueYear : Year) ->
               {auto 0 valid : So
                 (isValidWeekDate {pattern = Base16} week target valueYear)} ->
               CalendarDate IslamicBcl
fromWeekDate = fromWeekDate' {pattern = Base16}

||| Validate a runtime Islamic week date for the selected leap pattern.
public export
refineWeekDate' : {pattern : IslamicLeapPattern} ->
                  {auto known : KnownIslamicLeapPattern pattern} ->
                  WeekNumber -> IslamicDayOfWeek -> Year ->
                  Either IslamicDateError (CalendarDate (Islamic pattern))
refineWeekDate' @{known} week target valueYear =
  case choose (isValidWeekDate {pattern} week target valueYear) of
    Left valid => Right
      (fromWeekDate' {pattern} @{known} week target valueYear @{valid})
    Right _ => Left (InvalidIslamicWeekDate week target valueYear)

public export
refineWeekDate : WeekNumber -> IslamicDayOfWeek -> Year ->
                 Either IslamicDateError (CalendarDate IslamicBcl)
refineWeekDate = refineWeekDate' {pattern = Base16}

||| Construct a Saturday-based civil-epoch Islamic week date.
public export
civilFromWeekDate' : {pattern : IslamicLeapPattern} ->
                     {auto known : KnownIslamicLeapPattern pattern} ->
                     (week : WeekNumber) -> (target : IslamicDayOfWeek) ->
                     (valueYear : Year) ->
                     {auto 0 valid : So
                       (isValidCivilWeekDate {pattern} week target valueYear)} ->
                     CalendarDate (CivilIslamic pattern)
civilFromWeekDate' week target valueYear =
  makeIslamicDate {epoch = Civil} {pattern}
    (civilWeekDateDays {pattern} week target valueYear)

public export
civilFromWeekDate : (week : WeekNumber) ->
                    (target : IslamicDayOfWeek) -> (valueYear : Year) ->
                    {auto 0 valid : So
                      (isValidCivilWeekDate {pattern = Base16}
                        week target valueYear)} ->
                    CalendarDate CivilIslamicBcl
civilFromWeekDate = civilFromWeekDate' {pattern = Base16}

public export
refineCivilWeekDate' : {pattern : IslamicLeapPattern} ->
                       {auto known : KnownIslamicLeapPattern pattern} ->
                       WeekNumber -> IslamicDayOfWeek -> Year ->
                       Either IslamicDateError
                         (CalendarDate (CivilIslamic pattern))
refineCivilWeekDate' @{known} week target valueYear =
  case choose (isValidCivilWeekDate {pattern}
    week target valueYear) of
      Left valid => Right (civilFromWeekDate' {pattern} @{known}
        week target valueYear @{valid})
      Right _ => Left (InvalidIslamicWeekDate week target valueYear)

public export
refineCivilWeekDate : WeekNumber -> IslamicDayOfWeek -> Year ->
                      Either IslamicDateError (CalendarDate CivilIslamicBcl)
refineCivilWeekDate = refineCivilWeekDate' {pattern = Base16}
