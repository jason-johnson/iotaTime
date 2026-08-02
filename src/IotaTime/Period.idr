module IotaTime.Period

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

||| Types to which periods can be applied.
public export
interface ApplyPeriod target where
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
nanoseconds value = MkPeriod 0 0 0 0 0 0 0 value

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