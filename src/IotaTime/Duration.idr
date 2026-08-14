module IotaTime.Duration

%default total

||| A fixed amount of elapsed timeline time, measured in nanoseconds.
||| Unlike `Period`, a duration has no calendar-relative units.
export
record DurationRep where
  constructor MkDuration
  storedNanoseconds : Integer

||| The public opaque type of fixed elapsed durations.
public export
Duration : Type
Duration = DurationRep

public export
Eq Duration where
  left == right = left.storedNanoseconds == right.storedNanoseconds

public export
Ord Duration where
  compare left right = compare left.storedNanoseconds right.storedNanoseconds

public export
Show Duration where
  show value = "fromNanoseconds " ++ show value.storedNanoseconds

||| Construct a duration from an exact number of nanoseconds.
export
durationFromNanoseconds : Integer -> Duration
durationFromNanoseconds = MkDuration

||| Construct a duration from an exact number of nanoseconds.
public export
fromNanoseconds : Integer -> Duration
fromNanoseconds = durationFromNanoseconds

||| Construct a duration from an exact number of microseconds.
export
durationFromMicroseconds : Integer -> Duration
durationFromMicroseconds value = MkDuration (value * 1000)

||| Construct a duration from an exact number of microseconds.
public export
fromMicroseconds : Integer -> Duration
fromMicroseconds = durationFromMicroseconds

||| Construct a duration from an exact number of milliseconds.
export
durationFromMilliseconds : Integer -> Duration
durationFromMilliseconds value = MkDuration (value * 1000000)

||| Construct a duration from an exact number of milliseconds.
public export
fromMilliseconds : Integer -> Duration
fromMilliseconds = durationFromMilliseconds

||| Construct a duration from an exact number of seconds.
export
durationFromSeconds : Integer -> Duration
durationFromSeconds value = MkDuration (value * 1000000000)

||| Construct a duration from an exact number of seconds.
public export
fromSeconds : Integer -> Duration
fromSeconds = durationFromSeconds

||| Construct a duration from fixed 60-second minutes.
export
durationFromMinutes : Integer -> Duration
durationFromMinutes value = durationFromSeconds (value * 60)

||| Construct a duration from fixed 60-second minutes.
public export
fromMinutes : Integer -> Duration
fromMinutes = durationFromMinutes

||| Construct a duration from fixed 60-minute hours.
export
durationFromHours : Integer -> Duration
durationFromHours value = durationFromMinutes (value * 60)

||| Construct a duration from fixed 60-minute hours.
public export
fromHours : Integer -> Duration
fromHours = durationFromHours

||| Construct a duration from fixed 24-hour days, independent of calendars and zones.
export
durationFromStandardDays : Integer -> Duration
durationFromStandardDays value = durationFromHours (value * 24)

||| Construct a duration from fixed 24-hour days, independent of calendars and zones.
public export
fromStandardDays : Integer -> Duration
fromStandardDays = durationFromStandardDays

||| Construct a duration from fixed seven-day weeks.
export
durationFromStandardWeeks : Integer -> Duration
durationFromStandardWeeks value = durationFromStandardDays (value * 7)

||| Construct a duration from fixed seven-day weeks.
public export
fromStandardWeeks : Integer -> Duration
fromStandardWeeks = durationFromStandardWeeks

||| Return the exact signed nanosecond count represented by a duration.
public export
toDurationNanoseconds : Duration -> Integer
toDurationNanoseconds = storedNanoseconds

||| Return the number of whole microseconds in a duration.
public export
toMicroseconds : Duration -> Integer
toMicroseconds value = toDurationNanoseconds value `div` 1000

||| Return the number of whole milliseconds in a duration.
public export
toMilliseconds : Duration -> Integer
toMilliseconds value = toMicroseconds value `div` 1000

||| Return the number of whole seconds in a duration, truncated toward negative infinity.
public export
toSeconds : Duration -> Integer
toSeconds value = toMilliseconds value `div` 1000

||| Return the number of whole fixed 60-second minutes in a duration.
public export
toMinutes : Duration -> Integer
toMinutes value = toSeconds value `div` 60

||| Return the number of whole fixed 60-minute hours in a duration.
public export
toHours : Duration -> Integer
toHours value = toMinutes value `div` 60

||| Return the number of whole fixed 24-hour days in a duration.
public export
toStandardDays : Duration -> Integer
toStandardDays value = toHours value `div` 24

||| Return the number of whole fixed seven-day weeks in a duration.
public export
toStandardWeeks : Duration -> Integer
toStandardWeeks value = toStandardDays value `div` 7

||| The duration containing no elapsed time.
export
zeroDuration : Duration
zeroDuration = MkDuration 0

||| Add two elapsed durations.
export
addDurations : Duration -> Duration -> Duration
addDurations left right =
  MkDuration (left.storedNanoseconds + right.storedNanoseconds)

||| Add two elapsed durations.
public export
add : Duration -> Duration -> Duration
add = addDurations

||| Subtract the second duration from the first.
export
subtractDurations : Duration -> Duration -> Duration
subtractDurations left right =
  MkDuration (left.storedNanoseconds - right.storedNanoseconds)

||| Subtract the second duration from the first.
public export
minus : Duration -> Duration -> Duration
minus = subtractDurations

||| Reverse the direction of a duration.
export
negateDuration : Duration -> Duration
negateDuration value = MkDuration (negate value.storedNanoseconds)

||| Multiply a duration by an integer factor.
export
scaleDuration : Integer -> Duration -> Duration
scaleDuration factor value = MkDuration (factor * value.storedNanoseconds)

||| Proof that constructing then observing a nanosecond count is lossless.
public export
durationNanosecondsRoundTrip : (value : Integer) ->
  toDurationNanoseconds (fromNanoseconds value) = value
durationNanosecondsRoundTrip value = Refl

||| Proof that observing then reconstructing a duration preserves it.
public export
durationRoundTrip : (value : Duration) ->
  fromNanoseconds (toDurationNanoseconds value) = value
durationRoundTrip (MkDuration value) = Refl
