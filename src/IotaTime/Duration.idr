module IotaTime.Duration

%default total

||| A fixed amount of elapsed timeline time, measured in nanoseconds.
||| Unlike `Period`, a duration has no calendar-relative units.
export
record DurationRep where
  constructor MkDuration
  storedNanoseconds : Integer

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

public export
durationFromNanoseconds : Integer -> Duration
durationFromNanoseconds = MkDuration

public export
fromNanoseconds : Integer -> Duration
fromNanoseconds = durationFromNanoseconds

public export
durationFromMicroseconds : Integer -> Duration
durationFromMicroseconds value = MkDuration (value * 1000)

public export
fromMicroseconds : Integer -> Duration
fromMicroseconds = durationFromMicroseconds

public export
durationFromMilliseconds : Integer -> Duration
durationFromMilliseconds value = MkDuration (value * 1000000)

public export
fromMilliseconds : Integer -> Duration
fromMilliseconds = durationFromMilliseconds

public export
durationFromSeconds : Integer -> Duration
durationFromSeconds value = MkDuration (value * 1000000000)

public export
fromSeconds : Integer -> Duration
fromSeconds = durationFromSeconds

public export
durationFromMinutes : Integer -> Duration
durationFromMinutes value = durationFromSeconds (value * 60)

public export
fromMinutes : Integer -> Duration
fromMinutes = durationFromMinutes

public export
durationFromHours : Integer -> Duration
durationFromHours value = durationFromMinutes (value * 60)

public export
fromHours : Integer -> Duration
fromHours = durationFromHours

public export
durationFromStandardDays : Integer -> Duration
durationFromStandardDays value = durationFromHours (value * 24)

public export
fromStandardDays : Integer -> Duration
fromStandardDays = durationFromStandardDays

public export
durationFromStandardWeeks : Integer -> Duration
durationFromStandardWeeks value = durationFromStandardDays (value * 7)

public export
fromStandardWeeks : Integer -> Duration
fromStandardWeeks = durationFromStandardWeeks

public export
toDurationNanoseconds : Duration -> Integer
toDurationNanoseconds = storedNanoseconds

public export
zeroDuration : Duration
zeroDuration = MkDuration 0

public export
addDurations : Duration -> Duration -> Duration
addDurations left right =
  MkDuration (left.storedNanoseconds + right.storedNanoseconds)

public export
add : Duration -> Duration -> Duration
add = addDurations

public export
subtractDurations : Duration -> Duration -> Duration
subtractDurations left right =
  MkDuration (left.storedNanoseconds - right.storedNanoseconds)

public export
minus : Duration -> Duration -> Duration
minus = subtractDurations

public export
negateDuration : Duration -> Duration
negateDuration value = MkDuration (negate value.storedNanoseconds)

public export
scaleDuration : Integer -> Duration -> Duration
scaleDuration factor value = MkDuration (factor * value.storedNanoseconds)

public export
durationNanosecondsRoundTrip : (value : Integer) ->
  toDurationNanoseconds (durationFromNanoseconds value) = value
durationNanosecondsRoundTrip value = Refl

public export
durationRoundTrip : (value : Duration) ->
  durationFromNanoseconds (toDurationNanoseconds value) = value
durationRoundTrip (MkDuration value) = Refl
