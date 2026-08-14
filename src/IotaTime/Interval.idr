module IotaTime.Interval

import public Data.So
import public IotaTime.Duration
import public IotaTime.Instant
import Derive.Prelude

%language ElabReflection

%default total

||| A half-open interval `[start, end)` on the global timeline.
export
record IntervalRep where
  constructor MkInterval
  storedStart : Instant
  storedEnd : Instant
  0 valid : So
    (toNanosecondsSinceEpoch storedStart <= toNanosecondsSinceEpoch storedEnd)

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
  (rewrite instantNanosecondsRoundTrip startNanoseconds in
   rewrite instantNanosecondsRoundTrip endNanoseconds in valid)

checkedInterval : (start, end : Instant) ->
                  {auto 0 valid : So (isValidInterval start end)} -> Interval
checkedInterval start end = MkInterval start end valid

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
start (MkInterval value _ _) = value

public export
end : Interval -> Instant
end (MkInterval _ value _) = value

||| Every interval carries erased evidence that its endpoints are ordered.
public export
0 intervalIsValid : (value : Interval) ->
  So (isValidInterval (start value) (end value))
intervalIsValid (MkInterval _ _ valid) = valid

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
overlaps left right = not (isEmpty left) && not (isEmpty right) &&
  start left < end right && start right < end left

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
   in case choose (isValidInterval overlapStart overlapEnd) of
        Left valid => if overlapStart < overlapEnd
          then Just (MkInterval overlapStart overlapEnd valid)
          else Nothing
        Right _ => Nothing

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
        then let unionStart = min (start left) (start right)
                 unionEnd = max (end left) (end right)
              in case choose (isValidInterval unionStart unionEnd) of
                   Left valid => Just (MkInterval unionStart unionEnd valid)
                   Right _ => Nothing
        else Nothing

||| Return the nonnegative fixed duration between the endpoints.
public export
duration : Interval -> Duration
duration value = difference (end value) (start value)

%runElab derive `{IntervalRep} [Eq, Ord]

public export
Show IntervalRep where
  show value = "interval " ++ show (start value) ++ " " ++ show (end value)

||| A half-open timeline interval whose start, end, or both may be unbounded.
||| `Nothing` denotes negative infinity for the start and positive infinity for
||| the end.
||| Decide whether optional endpoints are ordered as a valid interval.
public export
isValidUnboundedInterval : Maybe Instant -> Maybe Instant -> Bool
isValidUnboundedInterval (Just start) (Just end) =
  isValidInterval start end
isValidUnboundedInterval _ _ = True

public export
isValidUnboundedNanosecondInterval : Maybe Integer -> Maybe Integer -> Bool
isValidUnboundedNanosecondInterval (Just start) (Just end) = start <= end
isValidUnboundedNanosecondInterval _ _ = True

export
record UnboundedIntervalRep where
  constructor MkUnboundedInterval
  storedUnboundedStart : Maybe Instant
  storedUnboundedEnd : Maybe Instant
  0 valid : So
    (isValidUnboundedInterval storedUnboundedStart storedUnboundedEnd)

public export
UnboundedInterval : Type
UnboundedInterval = UnboundedIntervalRep

||| Construct a statically validated interval with optional endpoints.
public export
unboundedInterval : (startNanoseconds, endNanoseconds : Maybe Integer) ->
                    {auto 0 valid : So (isValidUnboundedNanosecondInterval
                      startNanoseconds endNanoseconds)} ->
                    UnboundedInterval
unboundedInterval Nothing Nothing = MkUnboundedInterval Nothing Nothing Oh
unboundedInterval Nothing (Just end) = MkUnboundedInterval Nothing
  (Just (fromNanosecondsSinceEpoch end)) Oh
unboundedInterval (Just start) Nothing = MkUnboundedInterval
  (Just (fromNanosecondsSinceEpoch start)) Nothing Oh
unboundedInterval (Just start) (Just end) @{valid} = MkUnboundedInterval
  (Just (fromNanosecondsSinceEpoch start))
  (Just (fromNanosecondsSinceEpoch end))
  (rewrite instantNanosecondsRoundTrip start in
   rewrite instantNanosecondsRoundTrip end in valid)

||| Validate optional endpoints learned at runtime.
public export
refineUnboundedInterval : (start, end : Maybe Instant) ->
                          Either IntervalError UnboundedInterval
refineUnboundedInterval (Just start) (Just end) =
  case choose (isValidUnboundedInterval (Just start) (Just end)) of
    Left valid => Right (MkUnboundedInterval (Just start) (Just end) valid)
    Right _ => Left (ReversedInterval start end)
refineUnboundedInterval Nothing Nothing =
  Right (MkUnboundedInterval Nothing Nothing Oh)
refineUnboundedInterval Nothing (Just end) =
  Right (MkUnboundedInterval Nothing (Just end) Oh)
refineUnboundedInterval (Just start) Nothing =
  Right (MkUnboundedInterval (Just start) Nothing Oh)

public export
unboundedStart : UnboundedInterval -> Maybe Instant
unboundedStart (MkUnboundedInterval value _ _) = value

public export
unboundedEnd : UnboundedInterval -> Maybe Instant
unboundedEnd (MkUnboundedInterval _ value _) = value

||| Every unbounded interval carries erased evidence that its finite endpoints
||| are ordered.
public export
0 unboundedIntervalIsValid : (value : UnboundedInterval) ->
  So (isValidUnboundedInterval
    (unboundedStart value) (unboundedEnd value))
unboundedIntervalIsValid (MkUnboundedInterval _ _ valid) = valid

||| Treat a bounded interval as an interval with two finite bounds.
public export
toUnboundedInterval : Interval -> UnboundedInterval
toUnboundedInterval value = MkUnboundedInterval (Just (start value))
  (Just (end value)) (intervalIsValid value)

||| Recover a bounded interval only when both endpoints are finite.
public export
toBoundedInterval : UnboundedInterval -> Maybe Interval
toBoundedInterval
  (MkUnboundedInterval (Just start) (Just end) valid) =
    Just (MkInterval start end valid)
toBoundedInterval _ = Nothing

||| Test membership using half-open endpoint semantics at every finite bound.
public export
unboundedContains : UnboundedInterval -> Instant -> Bool
unboundedContains value instant =
  case (unboundedStart value, unboundedEnd value) of
    (Nothing, Nothing) => True
    (Nothing, Just end) => instant < end
    (Just start, Nothing) => start <= instant
    (Just start, Just end) => start <= instant && instant < end

||| Whether the interval contains no instants.
public export
unboundedIsEmpty : UnboundedInterval -> Bool
unboundedIsEmpty (MkUnboundedInterval (Just start) (Just end) _) = start == end
unboundedIsEmpty _ = False

endAfterStart : Maybe Instant -> Maybe Instant -> Bool
endAfterStart Nothing _ = True
endAfterStart _ Nothing = True
endAfterStart (Just end) (Just start) = start < end

||| Whether two unbounded intervals share at least one instant.
public export
unboundedOverlaps : UnboundedInterval -> UnboundedInterval -> Bool
unboundedOverlaps left right =
  not (unboundedIsEmpty left) && not (unboundedIsEmpty right) &&
  endAfterStart (unboundedEnd left) (unboundedStart right) &&
  endAfterStart (unboundedEnd right) (unboundedStart left)

finiteBoundsEqual : Maybe Instant -> Maybe Instant -> Bool
finiteBoundsEqual (Just left) (Just right) = left == right
finiteBoundsEqual _ _ = False

||| Whether two non-overlapping intervals touch at one finite endpoint.
public export
unboundedIsAdjacent : UnboundedInterval -> UnboundedInterval -> Bool
unboundedIsAdjacent left right =
  finiteBoundsEqual (unboundedEnd left) (unboundedStart right) ||
  finiteBoundsEqual (unboundedEnd right) (unboundedStart left)

laterStart : Maybe Instant -> Maybe Instant -> Maybe Instant
laterStart Nothing right = right
laterStart left Nothing = left
laterStart (Just left) (Just right) = Just (max left right)

earlierEnd : Maybe Instant -> Maybe Instant -> Maybe Instant
earlierEnd Nothing right = right
earlierEnd left Nothing = left
earlierEnd (Just left) (Just right) = Just (min left right)

earlierStart : Maybe Instant -> Maybe Instant -> Maybe Instant
earlierStart Nothing _ = Nothing
earlierStart _ Nothing = Nothing
earlierStart (Just left) (Just right) = Just (min left right)

laterEnd : Maybe Instant -> Maybe Instant -> Maybe Instant
laterEnd Nothing _ = Nothing
laterEnd _ Nothing = Nothing
laterEnd (Just left) (Just right) = Just (max left right)

||| Return the non-empty intersection of two intervals.
public export
unboundedIntersection : UnboundedInterval -> UnboundedInterval ->
                        Maybe UnboundedInterval
unboundedIntersection left right =
  if unboundedOverlaps left right
    then case refineUnboundedInterval
      (laterStart (unboundedStart left) (unboundedStart right))
      (earlierEnd (unboundedEnd left) (unboundedEnd right)) of
        Left _ => Nothing
        Right value => Just value
    else Nothing

||| Return the smallest interval containing both inputs when their union is
||| connected. Empty intervals are absorbed by the other input.
public export
unboundedUnion : UnboundedInterval -> UnboundedInterval ->
                 Maybe UnboundedInterval
unboundedUnion left right =
  if unboundedIsEmpty left
    then Just right
    else if unboundedIsEmpty right
      then Just left
      else if unboundedOverlaps left right || unboundedIsAdjacent left right
        then case refineUnboundedInterval
          (earlierStart (unboundedStart left) (unboundedStart right))
          (laterEnd (unboundedEnd left) (unboundedEnd right)) of
            Left _ => Nothing
            Right value => Just value
        else Nothing

||| Return the duration when both endpoints are finite.
public export
unboundedDuration : UnboundedInterval -> Maybe Duration
unboundedDuration (MkUnboundedInterval (Just start) (Just end) _) =
  Just (difference end start)
unboundedDuration _ = Nothing

public export
Eq UnboundedIntervalRep where
  left == right = unboundedStart left == unboundedStart right &&
    unboundedEnd left == unboundedEnd right

compareStarts : Maybe Instant -> Maybe Instant -> Ordering
compareStarts Nothing Nothing = EQ
compareStarts Nothing (Just _) = LT
compareStarts (Just _) Nothing = GT
compareStarts (Just left) (Just right) = compare left right

compareEnds : Maybe Instant -> Maybe Instant -> Ordering
compareEnds Nothing Nothing = EQ
compareEnds Nothing (Just _) = GT
compareEnds (Just _) Nothing = LT
compareEnds (Just left) (Just right) = compare left right

public export
Ord UnboundedIntervalRep where
  compare left right = case compareStarts
    (unboundedStart left) (unboundedStart right) of
      EQ => compareEnds (unboundedEnd left) (unboundedEnd right)
      result => result

public export
Show UnboundedIntervalRep where
  show value = "unboundedInterval " ++ show (unboundedStart value) ++ " " ++
    show (unboundedEnd value)