module IotaTime.Interval

import public Data.So
import public IotaTime.Duration
import public IotaTime.Instant

%default total

||| A half-open interval `[start, end)` on the global timeline.
export
record IntervalRep where
  constructor MkInterval
  storedStart : Instant
  storedEnd : Instant

public export
Interval : Type
Interval = IntervalRep

||| Decide whether two instants are ordered as a valid interval.
public export
isValidInterval : Instant -> Instant -> Bool
isValidInterval start end =
  toNanosecondsSinceEpoch start <= toNanosecondsSinceEpoch end

||| Construct a statically validated half-open interval from nanosecond counts
||| relative to the library epoch.
public export
interval : (startNanoseconds, endNanoseconds : Integer) ->
           {auto 0 valid : So (startNanoseconds <= endNanoseconds)} -> Interval
interval startNanoseconds endNanoseconds = MkInterval
  (fromNanosecondsSinceEpoch startNanoseconds)
  (fromNanosecondsSinceEpoch endNanoseconds)

checkedInterval : (start, end : Instant) ->
                  {auto 0 valid : So (isValidInterval start end)} -> Interval
checkedInterval start end = MkInterval start end

public export
data IntervalError = ReversedInterval Instant Instant

||| Validate arbitrary endpoints learned at runtime.
public export
refineInterval : (start, end : Instant) -> Either IntervalError Interval
refineInterval start end =
  case choose (isValidInterval start end) of
    Left valid => Right (checkedInterval start end @{valid})
    Right _ => Left (ReversedInterval start end)

public export
start : Interval -> Instant
start (MkInterval value _) = value

public export
end : Interval -> Instant
end (MkInterval _ value) = value

||| Test membership in the half-open interval `[start, end)`.
public export
contains : Interval -> Instant -> Bool
contains value instant = start value <= instant && instant < end value

||| Return the nonnegative fixed duration between the endpoints.
public export
duration : Interval -> Duration
duration value = difference (end value) (start value)

public export
Eq IntervalRep where
  left == right = start left == start right && end left == end right

public export
Show IntervalRep where
  show value = "interval " ++ show (start value) ++ " " ++ show (end value)