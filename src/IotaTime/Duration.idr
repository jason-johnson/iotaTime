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
  show value = "durationFromNanoseconds " ++ show value.storedNanoseconds

||| Construct a duration from an exact number of nanoseconds.
public export
durationFromNanoseconds : Integer -> Duration
durationFromNanoseconds = MkDuration

||| Alias for `durationFromNanoseconds`.
public export
fromNanoseconds : Integer -> Duration
fromNanoseconds = durationFromNanoseconds

||| Construct a duration from an exact number of microseconds.
public export
durationFromMicroseconds : Integer -> Duration
durationFromMicroseconds value = MkDuration (value * 1000)

||| Alias for `durationFromMicroseconds`.
public export
fromMicroseconds : Integer -> Duration
fromMicroseconds = durationFromMicroseconds

||| Construct a duration from an exact number of milliseconds.
public export
durationFromMilliseconds : Integer -> Duration
durationFromMilliseconds value = MkDuration (value * 1000000)

||| Alias for `durationFromMilliseconds`.
public export
fromMilliseconds : Integer -> Duration
fromMilliseconds = durationFromMilliseconds

||| Construct a duration from an exact number of seconds.
public export
durationFromSeconds : Integer -> Duration
durationFromSeconds value = MkDuration (value * 1000000000)

||| Alias for `durationFromSeconds`.
public export
fromSeconds : Integer -> Duration
fromSeconds = durationFromSeconds

||| Construct a duration from fixed 60-second minutes.
public export
durationFromMinutes : Integer -> Duration
durationFromMinutes value = durationFromSeconds (value * 60)

||| Alias for `durationFromMinutes`.
public export
fromMinutes : Integer -> Duration
fromMinutes = durationFromMinutes

||| Construct a duration from fixed 60-minute hours.
public export
durationFromHours : Integer -> Duration
durationFromHours value = durationFromMinutes (value * 60)

||| Alias for `durationFromHours`.
public export
fromHours : Integer -> Duration
fromHours = durationFromHours

||| Construct a duration from fixed 24-hour days, independent of calendars and zones.
public export
durationFromStandardDays : Integer -> Duration
durationFromStandardDays value = durationFromHours (value * 24)

||| Alias for `durationFromStandardDays`.
public export
fromStandardDays : Integer -> Duration
fromStandardDays = durationFromStandardDays

||| Construct a duration from fixed seven-day weeks.
public export
durationFromStandardWeeks : Integer -> Duration
durationFromStandardWeeks value = durationFromStandardDays (value * 7)

||| Alias for `durationFromStandardWeeks`.
public export
fromStandardWeeks : Integer -> Duration
fromStandardWeeks = durationFromStandardWeeks

||| Return the exact signed nanosecond count represented by a duration.
public export
toDurationNanoseconds : Duration -> Integer
toDurationNanoseconds = storedNanoseconds

||| The duration containing no elapsed time.
public export
zeroDuration : Duration
zeroDuration = MkDuration 0

||| Add two elapsed durations.
public export
addDurations : Duration -> Duration -> Duration
addDurations left right =
  MkDuration (left.storedNanoseconds + right.storedNanoseconds)

||| Alias for `addDurations`.
public export
add : Duration -> Duration -> Duration
add = addDurations

||| Subtract the second duration from the first.
public export
subtractDurations : Duration -> Duration -> Duration
subtractDurations left right =
  MkDuration (left.storedNanoseconds - right.storedNanoseconds)

||| Alias for `subtractDurations`.
public export
minus : Duration -> Duration -> Duration
minus = subtractDurations

||| Reverse the direction of a duration.
public export
negateDuration : Duration -> Duration
negateDuration value = MkDuration (negate value.storedNanoseconds)

||| Multiply a duration by an integer factor.
public export
scaleDuration : Integer -> Duration -> Duration
scaleDuration factor value = MkDuration (factor * value.storedNanoseconds)

||| Proof that constructing then observing a nanosecond count is lossless.
public export
durationNanosecondsRoundTrip : (value : Integer) ->
  toDurationNanoseconds (durationFromNanoseconds value) = value
durationNanosecondsRoundTrip value = Refl

||| Proof that observing then reconstructing a duration preserves it.
public export
durationRoundTrip : (value : Duration) ->
  durationFromNanoseconds (toDurationNanoseconds value) = value
durationRoundTrip (MkDuration value) = Refl
