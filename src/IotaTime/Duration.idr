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
durationFromMicroseconds : Integer -> Duration
durationFromMicroseconds value = MkDuration (value * 1000)

public export
durationFromMilliseconds : Integer -> Duration
durationFromMilliseconds value = MkDuration (value * 1000000)

public export
durationFromSeconds : Integer -> Duration
durationFromSeconds value = MkDuration (value * 1000000000)

public export
durationFromMinutes : Integer -> Duration
durationFromMinutes value = durationFromSeconds (value * 60)

public export
durationFromHours : Integer -> Duration
durationFromHours value = durationFromMinutes (value * 60)

public export
durationFromStandardDays : Integer -> Duration
durationFromStandardDays value = durationFromHours (value * 24)

public export
durationFromStandardWeeks : Integer -> Duration
durationFromStandardWeeks value = durationFromStandardDays (value * 7)

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
subtractDurations : Duration -> Duration -> Duration
subtractDurations left right =
  MkDuration (left.storedNanoseconds - right.storedNanoseconds)

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
