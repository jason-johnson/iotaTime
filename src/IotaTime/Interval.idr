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

||| Whether the interval contains no instants.
public export
isEmpty : Interval -> Bool
isEmpty value = start value == end value

||| Whether two half-open intervals share at least one instant.
public export
overlaps : Interval -> Interval -> Bool
overlaps left right = start left < end right && start right < end left

||| Whether two non-overlapping intervals touch at one endpoint.
public export
isAdjacent : Interval -> Interval -> Bool
isAdjacent left right = end left == start right || end right == start left

||| Return the non-empty intersection of two intervals.
public export
intersection : Interval -> Interval -> Maybe Interval
intersection left right =
  let overlapStart = max (start left) (start right)
      overlapEnd = min (end left) (end right)
   in if overlapStart < overlapEnd
        then Just (MkInterval overlapStart overlapEnd)
        else Nothing

||| Return the smallest interval containing both inputs when their union is
||| connected. Empty intervals are absorbed by the other input.
public export
union : Interval -> Interval -> Maybe Interval
union left right =
  if isEmpty left
    then Just right
    else if isEmpty right
      then Just left
      else if overlaps left right || isAdjacent left right
        then Just (MkInterval
          (min (start left) (start right))
          (max (end left) (end right)))
        else Nothing

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