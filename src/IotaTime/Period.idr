module IotaTime.Period

import IotaTime.Internal.ApplyPeriod
import Derive.Prelude

%language ElabReflection

%default total

||| A calendar-relative amount applicable to `target`.
||| The constructor is hidden so unit capabilities cannot be bypassed.
export
record Period (target : Type) where
  constructor MkPeriod
  storedYears : Integer
  storedMonths : Integer
  storedWeeks : Integer
  storedDays : Integer
  storedHours : Integer
  storedMinutes : Integer
  storedSeconds : Integer
  storedNanoseconds : Integer

export
periodYears : Period target -> Integer
periodYears (MkPeriod value _ _ _ _ _ _ _) = value

export
periodMonths : Period target -> Integer
periodMonths (MkPeriod _ value _ _ _ _ _ _) = value

export
periodWeeks : Period target -> Integer
periodWeeks (MkPeriod _ _ value _ _ _ _ _) = value

export
periodDays : Period target -> Integer
periodDays (MkPeriod _ _ _ value _ _ _ _) = value

export
periodHours : Period target -> Integer
periodHours (MkPeriod _ _ _ _ value _ _ _) = value

export
periodMinutes : Period target -> Integer
periodMinutes (MkPeriod _ _ _ _ _ value _ _) = value

export
periodSeconds : Period target -> Integer
periodSeconds (MkPeriod _ _ _ _ _ _ value _) = value

export
periodNanoseconds : Period target -> Integer
periodNanoseconds (MkPeriod _ _ _ _ _ _ _ value) = value

%runElab derive `{Period} [Eq]

public export
Show (Period target) where
  show value = "period " ++
    show (periodYears value) ++ " " ++
    show (periodMonths value) ++ " " ++
    show (periodWeeks value) ++ " " ++
    show (periodDays value) ++ " " ++
    show (periodHours value) ++ " " ++
    show (periodMinutes value) ++ " " ++
    show (periodSeconds value) ++ " " ++
    show (periodNanoseconds value)

emptyPeriod : Period target
emptyPeriod = MkPeriod 0 0 0 0 0 0 0 0

public export
Semigroup (Period target) where
  left <+> right = MkPeriod
    (periodYears left + periodYears right)
    (periodMonths left + periodMonths right)
    (periodWeeks left + periodWeeks right)
    (periodDays left + periodDays right)
    (periodHours left + periodHours right)
    (periodMinutes left + periodMinutes right)
    (periodSeconds left + periodSeconds right)
    (periodNanoseconds left + periodNanoseconds right)

public export
Monoid (Period target) where
  neutral = emptyPeriod

||| Types with calendar-relative date fields.
public export
interface HasCalendar target where
  0 calendarCapability : ()

||| Types with local time-of-day fields.
public export
interface HasTime target where
  0 timeCapability : ()

||| Library-owned types to which periods can be applied. The internal target
||| capability seals this interface against client implementations.
public export
interface PeriodTarget target => ApplyPeriod target where
  ||| Apply all components of a period using iotaTime's rules for the target.
  |||
  ||| For calendar dates, components apply from largest to smallest: years,
  ||| months, weeks, then days. Year and month shifts clamp an invalid day to
  ||| the target month's final day; every shift also clamps at the concrete
  ||| calendar's supported boundaries. Weeks are seven-day shifts. Components
  ||| combined with `<+>` are aggregated before this sequence, so
  ||| `months 1 <+> months 1` applies one two-month shift rather than two
  ||| separately clamped one-month shifts.
  |||
  ||| For `LocalTime`, clock components combine into one signed displacement
  ||| and wrap within the 24-hour day. For `CalendarDateTime`, date components
  ||| apply first as above, then clock components combine into one displacement
  ||| whose signed day carry adjusts the resulting date.
  applyPeriod : Period target -> target -> target

||| Construct a period measured in calendar years.
public export
years : HasCalendar target => Integer -> Period target
years value = MkPeriod value 0 0 0 0 0 0 0

||| Construct a period measured in calendar months.
public export
months : HasCalendar target => Integer -> Period target
months value = MkPeriod 0 value 0 0 0 0 0 0

||| Construct a period measured in seven-day calendar weeks.
public export
weeks : HasCalendar target => Integer -> Period target
weeks value = MkPeriod 0 0 value 0 0 0 0 0

||| Construct a period measured in calendar days.
public export
days : HasCalendar target => Integer -> Period target
days value = MkPeriod 0 0 0 value 0 0 0 0

||| Construct a period measured in hours.
public export
hours : HasTime target => Integer -> Period target
hours value = MkPeriod 0 0 0 0 value 0 0 0

||| Construct a period measured in minutes.
public export
minutes : HasTime target => Integer -> Period target
minutes value = MkPeriod 0 0 0 0 0 value 0 0

||| Construct a period measured in seconds.
public export
seconds : HasTime target => Integer -> Period target
seconds value = MkPeriod 0 0 0 0 0 0 value 0

||| Construct a period measured in nanoseconds.
public export
nanoseconds : HasTime target => Integer -> Period target
nanoseconds = MkPeriod 0 0 0 0 0 0 0

||| Negate every component of a period.
public export
negatePeriod : Period target -> Period target
negatePeriod period = MkPeriod
  (negate (periodYears period))
  (negate (periodMonths period))
  (negate (periodWeeks period))
  (negate (periodDays period))
  (negate (periodHours period))
  (negate (periodMinutes period))
  (negate (periodSeconds period))
  (negate (periodNanoseconds period))

||| Multiply every component of a period by an integer.
public export
scalePeriod : Integer -> Period target -> Period target
scalePeriod factor period = MkPeriod
  (factor * periodYears period)
  (factor * periodMonths period)
  (factor * periodWeeks period)
  (factor * periodDays period)
  (factor * periodHours period)
  (factor * periodMinutes period)
  (factor * periodSeconds period)
  (factor * periodNanoseconds period)