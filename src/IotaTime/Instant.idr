module IotaTime.Instant

import IotaTime.Clock
import IotaTime.Duration

%default total

nanosecondsPerSecond : Integer
nanosecondsPerSecond = 1000000000

secondsPerDay : Integer
secondsPerDay = 86400

unixEpochOffsetDays : Integer
unixEpochOffsetDays = 11017

unixEpochOffsetNanoseconds : Integer
unixEpochOffsetNanoseconds =
  unixEpochOffsetDays * secondsPerDay * nanosecondsPerSecond

||| A fixed point on the global timeline, measured in nanoseconds from
||| March 1, 2000 at 00:00:00 UTC.
|||
||| Representation benchmark note: if profiling shows scalar `Integer`
||| arithmetic to be costly, compare it with a proof-oriented record of an
||| `Integer` day, `Fin 86400` second-of-day, and `Fin 1000000000` nanosecond.
||| The scalar is preferred until then because canonical form is structural
||| and arithmetic cannot overflow or require normalization proofs.
export
record Instant where
  constructor MkInstant
  storedNanoseconds : Integer

public export
Eq Instant where
  left == right = left.storedNanoseconds == right.storedNanoseconds

public export
Ord Instant where
  compare left right = compare left.storedNanoseconds right.storedNanoseconds

public export
Show Instant where
  show value = "fromNanosecondsSinceEpoch " ++ show value.storedNanoseconds

||| Construct an instant from nanoseconds relative to the library epoch.
public export
fromNanosecondsSinceEpoch : Integer -> Instant
fromNanosecondsSinceEpoch = MkInstant

||| Read an instant as nanoseconds relative to the library epoch.
public export
toNanosecondsSinceEpoch : Instant -> Integer
toNanosecondsSinceEpoch = storedNanoseconds

||| Backward-compatible name for `toNanosecondsSinceEpoch`.
public export
ticks : Instant -> Integer
ticks = toNanosecondsSinceEpoch

||| Converting a scalar nanosecond count to an instant and back is exact.
public export
instantNanosecondsRoundTrip : (value : Integer) ->
  toNanosecondsSinceEpoch (fromNanosecondsSinceEpoch value) = value
instantNanosecondsRoundTrip value = Refl

||| Reconstructing an instant from its scalar nanosecond count is exact.
public export
instantRoundTrip : (value : Instant) ->
  fromNanosecondsSinceEpoch (toNanosecondsSinceEpoch value) = value
instantRoundTrip (MkInstant value) = Refl

||| Construct an instant from whole seconds relative to the Unix epoch.
public export
fromSecondsSinceUnixEpoch : Integer -> Instant
fromSecondsSinceUnixEpoch seconds =
  MkInstant (seconds * nanosecondsPerSecond - unixEpochOffsetNanoseconds)

||| Construct an instant from nanoseconds relative to the Unix epoch.
public export
fromNanosecondsSinceUnixEpoch : Integer -> Instant
fromNanosecondsSinceUnixEpoch value =
  MkInstant (value - unixEpochOffsetNanoseconds)

||| Read an instant as nanoseconds relative to the Unix epoch.
public export
toNanosecondsSinceUnixEpoch : Instant -> Integer
toNanosecondsSinceUnixEpoch value =
  value.storedNanoseconds + unixEpochOffsetNanoseconds

||| Add a fixed duration to an instant.
public export
addDuration : Instant -> Duration -> Instant
addDuration instant duration = MkInstant
  (instant.storedNanoseconds +
    toDurationNanoseconds duration)

public export
add : Instant -> Duration -> Instant
add = addDuration

||| Subtract a fixed duration from an instant.
public export
subtractDuration : Instant -> Duration -> Instant
subtractDuration instant duration = MkInstant
  (instant.storedNanoseconds -
    toDurationNanoseconds duration)

public export
minus : Instant -> Duration -> Instant
minus = subtractDuration

||| Compute the signed fixed duration from the second instant to the first.
public export
difference : Instant -> Instant -> Duration
difference left right = durationFromNanoseconds
  (left.storedNanoseconds - right.storedNanoseconds)

||| Read the current UTC system clock as an instant.
public export
now : IO Instant
now = do
  current <- currentUnixNanoseconds
  pure (fromNanosecondsSinceUnixEpoch current)

||| The library epoch: March 1, 2000 at 00:00:00 UTC.
public export
epoch : Instant
epoch = MkInstant 0