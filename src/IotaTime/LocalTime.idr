module IotaTime.LocalTime

import public IotaTime.Time.Component
import IotaTime.Period

%default total

nanosPerSecond : Integer
nanosPerSecond = 1000000000

nanosPerDay : Integer
nanosPerDay = 86400 * nanosPerSecond

||| An opaque time of day with nanosecond precision.
export
record LocalTime where
  constructor MkLocalTime
  nanosSinceMidnight : Integer

public export
Eq LocalTime where
  left == right = left.nanosSinceMidnight == right.nanosSinceMidnight

public export
Ord LocalTime where
  compare left right = compare left.nanosSinceMidnight right.nanosSinceMidnight

||| Construct a local time from already refined components.
public export
localTime : Hour -> Minute -> Second -> Nanosecond -> LocalTime
localTime valueHour valueMinute valueSecond valueNanosecond = MkLocalTime
  (((hourValue valueHour * 60 + minuteValue valueMinute) * 60 + secondValue valueSecond) *
    nanosPerSecond + nanosecondValue valueNanosecond)

||| Extract the hour in the range 0-23.
public export
hour : LocalTime -> Hour
hour value = hourFromInteger (value.nanosSinceMidnight `div` (3600 * nanosPerSecond))

||| Extract the minute in the range 0-59.
public export
minute : LocalTime -> Minute
minute value = minuteFromInteger
  (value.nanosSinceMidnight `div` (60 * nanosPerSecond) `mod` 60)

||| Extract the second in the range 0-59.
public export
second : LocalTime -> Second
second value = secondFromInteger (value.nanosSinceMidnight `div` nanosPerSecond `mod` 60)

||| Extract the nanosecond within the current second.
public export
nanosecond : LocalTime -> Nanosecond
nanosecond value = nanosecondFromInteger (value.nanosSinceMidnight `mod` nanosPerSecond)

||| Identifies which raw local-time component failed refinement.
public export
data LocalTimeError
  = InvalidHour HourError
  | InvalidMinute MinuteError
  | InvalidSecond SecondError
  | InvalidNanosecond NanosecondError

||| Validate raw hour, minute, second, and nanosecond values as a local time.
public export
refineLocalTime : Integer -> Integer -> Integer -> Integer -> Either LocalTimeError LocalTime
refineLocalTime rawHour rawMinute rawSecond rawNanosecond = do
  valueHour <- mapFst InvalidHour (refineHour rawHour)
  valueMinute <- mapFst InvalidMinute (refineMinute rawMinute)
  valueSecond <- mapFst InvalidSecond (refineSecond rawSecond)
  valueNanosecond <- mapFst InvalidNanosecond (refineNanosecond rawNanosecond)
  pure (localTime valueHour valueMinute valueSecond valueNanosecond)

export
applyTimePeriodWithCarry : Period target -> LocalTime -> (Integer, LocalTime)
applyTimePeriodWithCarry period value =
  let combined = value.nanosSinceMidnight + delta
      carry = combined `div` nanosPerDay
      withinDay = combined `mod` nanosPerDay
   in (carry, MkLocalTime withinDay)
  where
    delta = (((periodHours period * 60 + periodMinutes period) * 60 +
      periodSeconds period) * nanosPerSecond) + periodNanoseconds period

public export
HasTime LocalTime where
  timeCapability = ()

public export
ApplyPeriod LocalTime where
  applyPeriod period = snd . applyTimePeriodWithCarry period