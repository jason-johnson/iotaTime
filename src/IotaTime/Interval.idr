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
isEmpty value =
  toNanosecondsSinceEpoch (start value) ==
  toNanosecondsSinceEpoch (end value)

||| Whether two half-open intervals share at least one instant.
public export
overlaps : Interval -> Interval -> Bool
overlaps left right = not (isEmpty left) && not (isEmpty right) &&
  toNanosecondsSinceEpoch (start left) <
    toNanosecondsSinceEpoch (end right) &&
  toNanosecondsSinceEpoch (start right) <
    toNanosecondsSinceEpoch (end left)

||| Whether two non-overlapping intervals touch at one endpoint.
public export
isAdjacent : Interval -> Interval -> Bool
isAdjacent left right =
  toNanosecondsSinceEpoch (end left) ==
    toNanosecondsSinceEpoch (start right) ||
  toNanosecondsSinceEpoch (end right) ==
    toNanosecondsSinceEpoch (start left)

||| The later start bound selected for an intersection.
public export
intersectionStart : Interval -> Interval -> Instant
intersectionStart left right = max (start left) (start right)

||| The earlier end bound selected for an intersection.
public export
intersectionEnd : Interval -> Interval -> Instant
intersectionEnd left right = min (end left) (end right)

||| Whether two intervals have a valid, non-empty intersection.
public export
hasNonEmptyIntersection : Interval -> Interval -> Bool
hasNonEmptyIntersection left right =
  isValidInterval (intersectionStart left right) (intersectionEnd left right) &&
  toNanosecondsSinceEpoch (intersectionStart left right) <
    toNanosecondsSinceEpoch (intersectionEnd left right)

0 andLeft : (left, right : Bool) -> So (left && right) -> So left
andLeft True True Oh = Oh

0 andRight : (left, right : Bool) -> So (left && right) -> So right
andRight True True Oh = Oh

||| Return the non-empty intersection when its existence is statically known.
public export
intersection : (left, right : Interval) ->
               {auto 0 intersects : So
                 (hasNonEmptyIntersection left right)} ->
               Interval
intersection left right @{intersects} = MkInterval
  (intersectionStart left right)
  (intersectionEnd left right)
  (andLeft
    (isValidInterval
      (intersectionStart left right) (intersectionEnd left right))
    (toNanosecondsSinceEpoch (intersectionStart left right) <
      toNanosecondsSinceEpoch (intersectionEnd left right))
    intersects)

public export
data IntersectionError = NoNonEmptyIntersection

||| Return the non-empty intersection of intervals learned at runtime.
public export
refineIntersection : (left, right : Interval) ->
                     Either IntersectionError Interval
refineIntersection left right =
  case choose (hasNonEmptyIntersection left right) of
    Left intersects => Right (intersection left right @{intersects})
    Right _ => Left NoNonEmptyIntersection

||| The start bound selected for a connected union.
public export
unionStart : Interval -> Interval -> Instant
unionStart left right =
  if isEmpty left then start right
  else if isEmpty right then start left
  else if toNanosecondsSinceEpoch (start left) <=
      toNanosecondsSinceEpoch (start right)
    then start left
    else start right

||| The end bound selected for a connected union.
public export
unionEnd : Interval -> Interval -> Instant
unionEnd left right =
  if isEmpty left then end right
  else if isEmpty right then end left
  else if toNanosecondsSinceEpoch (end left) >=
      toNanosecondsSinceEpoch (end right)
    then end left
    else end right

||| Whether the union of two intervals is connected.
connectedRelationship : Interval -> Interval -> Bool
connectedRelationship left right =
  isEmpty left || isEmpty right || overlaps left right || isAdjacent left right

public export
isConnected : Interval -> Interval -> Bool
isConnected left right = connectedRelationship left right &&
  isValidInterval (unionStart left right) (unionEnd left right)

||| Return the smallest interval containing both inputs when their union is
||| statically known to be connected. Empty intervals are absorbed by the
||| other input.
public export
union : (left, right : Interval) ->
        {auto 0 connected : So (isConnected left right)} ->
        Interval
union left right @{connected} = MkInterval
  (unionStart left right)
  (unionEnd left right)
  (andRight
    (connectedRelationship left right)
    (isValidInterval (unionStart left right) (unionEnd left right))
    connected)

public export
data UnionError = DisconnectedIntervals

||| Return the connected union of intervals learned at runtime.
public export
refineUnion : (left, right : Interval) -> Either UnionError Interval
refineUnion left right = case choose (isConnected left right) of
  Left connected => Right (union left right @{connected})
  Right _ => Left DisconnectedIntervals

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
unboundedIsEmpty (MkUnboundedInterval (Just start) (Just end) _) =
  toNanosecondsSinceEpoch start == toNanosecondsSinceEpoch end
unboundedIsEmpty _ = False

||| Whether an optional end lies after an optional start, treating `Nothing`
||| as the appropriate infinity.
public export
unboundedEndAfterStart : Maybe Instant -> Maybe Instant -> Bool
unboundedEndAfterStart Nothing _ = True
unboundedEndAfterStart _ Nothing = True
unboundedEndAfterStart (Just end) (Just start) =
  toNanosecondsSinceEpoch start < toNanosecondsSinceEpoch end

||| Whether two unbounded intervals share at least one instant.
public export
unboundedOverlaps : UnboundedInterval -> UnboundedInterval -> Bool
unboundedOverlaps left right =
  not (unboundedIsEmpty left) && not (unboundedIsEmpty right) &&
  unboundedEndAfterStart (unboundedEnd left) (unboundedStart right) &&
  unboundedEndAfterStart (unboundedEnd right) (unboundedStart left)

||| Whether two optional bounds are finite and equal.
public export
finiteUnboundedBoundsEqual : Maybe Instant -> Maybe Instant -> Bool
finiteUnboundedBoundsEqual (Just left) (Just right) =
  toNanosecondsSinceEpoch left == toNanosecondsSinceEpoch right
finiteUnboundedBoundsEqual _ _ = False

||| Whether two non-overlapping intervals touch at one finite endpoint.
public export
unboundedIsAdjacent : UnboundedInterval -> UnboundedInterval -> Bool
unboundedIsAdjacent left right =
  finiteUnboundedBoundsEqual (unboundedEnd left) (unboundedStart right) ||
  finiteUnboundedBoundsEqual (unboundedEnd right) (unboundedStart left)

laterStart : Maybe Instant -> Maybe Instant -> Maybe Instant
laterStart Nothing right = right
laterStart left Nothing = left
laterStart (Just left) (Just right) = Just
  (if toNanosecondsSinceEpoch left >= toNanosecondsSinceEpoch right
    then left else right)

earlierEnd : Maybe Instant -> Maybe Instant -> Maybe Instant
earlierEnd Nothing right = right
earlierEnd left Nothing = left
earlierEnd (Just left) (Just right) = Just
  (if toNanosecondsSinceEpoch left <= toNanosecondsSinceEpoch right
    then left else right)

earlierStart : Maybe Instant -> Maybe Instant -> Maybe Instant
earlierStart Nothing _ = Nothing
earlierStart _ Nothing = Nothing
earlierStart (Just left) (Just right) = Just
  (if toNanosecondsSinceEpoch left <= toNanosecondsSinceEpoch right
    then left else right)

laterEnd : Maybe Instant -> Maybe Instant -> Maybe Instant
laterEnd Nothing _ = Nothing
laterEnd _ Nothing = Nothing
laterEnd (Just left) (Just right) = Just
  (if toNanosecondsSinceEpoch left >= toNanosecondsSinceEpoch right
    then left else right)

||| The later optional start bound selected for an intersection.
public export
unboundedIntersectionStart : UnboundedInterval -> UnboundedInterval ->
                             Maybe Instant
unboundedIntersectionStart left right =
  laterStart (unboundedStart left) (unboundedStart right)

||| The earlier optional end bound selected for an intersection.
public export
unboundedIntersectionEnd : UnboundedInterval -> UnboundedInterval ->
                           Maybe Instant
unboundedIntersectionEnd left right =
  earlierEnd (unboundedEnd left) (unboundedEnd right)

||| Whether two unbounded intervals have a valid, non-empty intersection.
public export
hasNonEmptyUnboundedIntersection : UnboundedInterval ->
                                   UnboundedInterval -> Bool
hasNonEmptyUnboundedIntersection left right =
  isValidUnboundedInterval
    (unboundedIntersectionStart left right)
    (unboundedIntersectionEnd left right) &&
  unboundedOverlaps left right

||| Return the non-empty intersection when its existence is statically known.
public export
unboundedIntersection : (left, right : UnboundedInterval) ->
  {auto 0 intersects : So
    (hasNonEmptyUnboundedIntersection left right)} ->
  UnboundedInterval
unboundedIntersection left right @{intersects} = MkUnboundedInterval
  (unboundedIntersectionStart left right)
  (unboundedIntersectionEnd left right)
  (andLeft
    (isValidUnboundedInterval
      (unboundedIntersectionStart left right)
      (unboundedIntersectionEnd left right))
    (unboundedOverlaps left right)
    intersects)

||| Return the non-empty intersection of unbounded intervals learned at
||| runtime.
public export
refineUnboundedIntersection : (left, right : UnboundedInterval) ->
  Either IntersectionError UnboundedInterval
refineUnboundedIntersection left right =
  case choose (hasNonEmptyUnboundedIntersection left right) of
    Left intersects =>
      Right (unboundedIntersection left right @{intersects})
    Right _ => Left NoNonEmptyIntersection

||| The optional start bound selected for a connected union.
public export
unboundedUnionStart : UnboundedInterval -> UnboundedInterval -> Maybe Instant
unboundedUnionStart left right =
  if unboundedIsEmpty left then unboundedStart right
  else if unboundedIsEmpty right then unboundedStart left
  else earlierStart (unboundedStart left) (unboundedStart right)

||| The optional end bound selected for a connected union.
public export
unboundedUnionEnd : UnboundedInterval -> UnboundedInterval -> Maybe Instant
unboundedUnionEnd left right =
  if unboundedIsEmpty left then unboundedEnd right
  else if unboundedIsEmpty right then unboundedEnd left
  else laterEnd (unboundedEnd left) (unboundedEnd right)

||| Whether the union of two unbounded intervals is connected.
unboundedConnectedRelationship : UnboundedInterval ->
                                 UnboundedInterval -> Bool
unboundedConnectedRelationship left right =
  unboundedIsEmpty left || unboundedIsEmpty right ||
  unboundedOverlaps left right || unboundedIsAdjacent left right

public export
unboundedIsConnected : UnboundedInterval -> UnboundedInterval -> Bool
unboundedIsConnected left right = unboundedConnectedRelationship left right &&
  isValidUnboundedInterval
    (unboundedUnionStart left right) (unboundedUnionEnd left right)

||| Return the smallest interval containing both inputs when their union is
||| statically known to be connected. Empty intervals are absorbed by the
||| other input.
public export
unboundedUnion : (left, right : UnboundedInterval) ->
  {auto 0 connected : So (unboundedIsConnected left right)} ->
  UnboundedInterval
unboundedUnion left right @{connected} = MkUnboundedInterval
  (unboundedUnionStart left right)
  (unboundedUnionEnd left right)
  (andRight
    (unboundedConnectedRelationship left right)
    (isValidUnboundedInterval
      (unboundedUnionStart left right) (unboundedUnionEnd left right))
    connected)

||| Return the connected union of unbounded intervals learned at runtime.
public export
refineUnboundedUnion : (left, right : UnboundedInterval) ->
  Either UnionError UnboundedInterval
refineUnboundedUnion left right =
  case choose (unboundedIsConnected left right) of
    Left connected => Right (unboundedUnion left right @{connected})
    Right _ => Left DisconnectedIntervals

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