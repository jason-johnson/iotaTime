module Test.Interval

import IotaTime
import Test.Support

spanningZero : Interval
spanningZero = interval (-10) 10

emptyInterval : Interval
emptyInterval = interval 5 5

leftInterval : Interval
leftInterval = interval 0 10

overlappingInterval : Interval
overlappingInterval = interval 5 15

adjacentInterval : Interval
adjacentInterval = interval 10 20

separateInterval : Interval
separateInterval = interval 11 20

allTime : UnboundedInterval
allTime = unboundedInterval Nothing Nothing

fromZero : UnboundedInterval
fromZero = unboundedInterval (Just 0) Nothing

untilTen : UnboundedInterval
untilTen = unboundedInterval Nothing (Just 10)

finiteUnbounded : UnboundedInterval
finiteUnbounded = unboundedInterval (Just 0) (Just 10)

0 finiteUnboundedIsValid : So (isValidUnboundedInterval
  (unboundedStart Test.Interval.finiteUnbounded)
  (unboundedEnd Test.Interval.finiteUnbounded))
finiteUnboundedIsValid = unboundedIntervalIsValid finiteUnbounded

emptyUnbounded : UnboundedInterval
emptyUnbounded = unboundedInterval (Just 0) (Just 0)

proofDirectedIntersection : (left, right : Interval) ->
  {auto 0 intersects : So (hasNonEmptyIntersection left right)} -> Interval
proofDirectedIntersection = intersection

proofDirectedUnion : (left, right : Interval) ->
  {auto 0 connected : So (isConnected left right)} ->
  Interval
proofDirectedUnion = IotaTime.Interval.union

intervalCases : List RuntimeCase
intervalCases =
  [ MkRuntimeCase "static interval preserves its endpoints"
      (toNanosecondsSinceEpoch (start spanningZero) == -10 &&
        toNanosecondsSinceEpoch (end spanningZero) == 10)
  , MkRuntimeCase "interval includes its start"
      (contains spanningZero (fromNanosecondsSinceEpoch (-10)))
  , MkRuntimeCase "interval includes an interior instant"
      (contains spanningZero epoch)
  , MkRuntimeCase "interval excludes its end"
      (not (contains spanningZero (fromNanosecondsSinceEpoch 10)))
  , MkRuntimeCase "interval excludes values before its start"
      (not (contains spanningZero (fromNanosecondsSinceEpoch (-11))))
    , MkRuntimeCase "interval ordering is lexicographic by endpoints"
      (interval 0 9 < interval 0 10 && interval 0 10 < interval 1 2)
  , MkRuntimeCase "empty interval contains no instant"
      (not (contains emptyInterval (fromNanosecondsSinceEpoch 5)))
    , MkRuntimeCase "empty interval is identified"
      (isEmpty emptyInterval && not (isEmpty spanningZero))
    , MkRuntimeCase "overlap follows half-open endpoint semantics"
      (overlaps leftInterval overlappingInterval &&
      not (overlaps leftInterval adjacentInterval))
    , MkRuntimeCase "empty intervals never overlap"
      (not (overlaps emptyInterval leftInterval) &&
      not (overlaps leftInterval emptyInterval))
    , MkRuntimeCase "adjacent intervals touch without overlapping"
      (isAdjacent leftInterval adjacentInterval &&
      not (isAdjacent leftInterval separateInterval))
    , MkRuntimeCase "runtime intersection returns the shared non-empty range"
      (case refineIntersection leftInterval overlappingInterval of
        Right value => value == interval 5 10
        Left _ => False)
    , MkRuntimeCase "runtime intersection rejects adjacent intervals"
      (case refineIntersection leftInterval adjacentInterval of
        Left NoNonEmptyIntersection => True
        Right _ => False)
    , MkRuntimeCase "runtime connected union spans adjacent intervals"
      (case refineUnion leftInterval adjacentInterval of
        Right value => value == interval 0 20
        Left _ => False)
    , MkRuntimeCase "runtime connected union spans overlapping intervals"
      (case refineUnion leftInterval overlappingInterval of
        Right value => value == interval 0 15
        Left _ => False)
    , MkRuntimeCase "runtime union rejects separated intervals"
      (case refineUnion leftInterval separateInterval of
        Left DisconnectedIntervals => True
        Right _ => False)
    , MkRuntimeCase "runtime connected union absorbs an empty interval"
      (case refineUnion emptyInterval leftInterval of
        Right value => value == leftInterval
        Left _ => False)
  , MkRuntimeCase "interval duration is the endpoint difference"
      (duration spanningZero == IotaTime.Duration.fromNanoseconds 20)
  , MkRuntimeCase "empty interval has zero duration"
      (duration emptyInterval == IotaTime.Duration.fromNanoseconds 0)
  , MkRuntimeCase "dynamic ordered endpoints are accepted"
      (case refineInterval
        (fromNanosecondsSinceEpoch 20)
        (fromNanosecondsSinceEpoch 30) of
          Right value => duration value == IotaTime.Duration.fromNanoseconds 10
          Left _ => False)
  , MkRuntimeCase "dynamic reversed endpoints are rejected"
      (let later = fromNanosecondsSinceEpoch 30
           earlier = fromNanosecondsSinceEpoch 20
        in case refineInterval later earlier of
             Left (ReversedInterval actualStart actualEnd) =>
               actualStart == later && actualEnd == earlier
             Right _ => False)
  , MkRuntimeCase "large interval bounds do not overflow"
      (duration
        (interval
          (-999999999999999999999999999999)
          999999999999999999999999999999) ==
        IotaTime.Duration.fromNanoseconds 1999999999999999999999999999998)
  , MkRuntimeCase "fully unbounded interval contains every tested instant"
      (unboundedContains allTime
        (fromNanosecondsSinceEpoch (-999999999999999999999999999999)) &&
      unboundedContains allTime
        (fromNanosecondsSinceEpoch 999999999999999999999999999999))
  , MkRuntimeCase "finite unbounded bounds retain half-open semantics"
      (unboundedContains finiteUnbounded epoch &&
      unboundedContains finiteUnbounded (fromNanosecondsSinceEpoch 9) &&
      not (unboundedContains finiteUnbounded
        (fromNanosecondsSinceEpoch 10)))
  , MkRuntimeCase "one-sided intervals enforce only their finite bound"
      (not (unboundedContains fromZero (fromNanosecondsSinceEpoch (-1))) &&
      unboundedContains fromZero epoch &&
      unboundedContains untilTen (fromNanosecondsSinceEpoch (-100)) &&
      not (unboundedContains untilTen (fromNanosecondsSinceEpoch 10)))
  , MkRuntimeCase "only equal finite bounds form an empty interval"
      (unboundedIsEmpty emptyUnbounded &&
      not (unboundedIsEmpty allTime) &&
      not (unboundedIsEmpty fromZero))
  , MkRuntimeCase "bounded and unbounded interval conversions round trip"
      (toBoundedInterval (toUnboundedInterval leftInterval) ==
        Just leftInterval &&
      toBoundedInterval allTime == Nothing)
    , MkRuntimeCase "runtime unbounded intersection selects tighter bounds"
        (case refineUnboundedIntersection fromZero untilTen of
          Right value => value == finiteUnbounded
          Left _ => False)
    , MkRuntimeCase "runtime unbounded union extends through infinite bounds"
        (case refineUnboundedUnion fromZero untilTen of
          Right value => value == allTime
          Left _ => False)
  , MkRuntimeCase "unbounded adjacency uses finite touching endpoints"
      (let untilZero = unboundedInterval Nothing (Just 0)
        in unboundedIsAdjacent untilZero fromZero &&
          unboundedIsAdjacent fromZero untilZero &&
      not (unboundedOverlaps
        untilZero fromZero))
  , MkRuntimeCase "empty unbounded intervals never overlap"
      (not (unboundedOverlaps emptyUnbounded finiteUnbounded) &&
      not (unboundedOverlaps finiteUnbounded emptyUnbounded))
  , MkRuntimeCase "unbounded ordering uses endpoint infinity semantics"
      (untilTen < allTime && allTime < finiteUnbounded &&
      finiteUnbounded < fromZero)
  , MkRuntimeCase "separated unbounded intervals have no connected union"
      (let untilZero = unboundedInterval Nothing (Just 0)
           afterOne = unboundedInterval (Just 1) Nothing
    in case (refineUnboundedIntersection untilZero afterOne,
       refineUnboundedUnion untilZero afterOne) of
      (Left NoNonEmptyIntersection, Left DisconnectedIntervals) => True
      _ => False)
  , MkRuntimeCase "unbounded duration exists only for finite endpoints"
      (unboundedDuration finiteUnbounded ==
        Just (IotaTime.Duration.fromNanoseconds 10) &&
      unboundedDuration allTime == Nothing)
  , MkRuntimeCase "dynamic reversed unbounded endpoints are rejected"
      (let later = fromNanosecondsSinceEpoch 30
           earlier = fromNanosecondsSinceEpoch 20
        in case refineUnboundedInterval (Just later) (Just earlier) of
             Left (ReversedInterval actualStart actualEnd) =>
               actualStart == later && actualEnd == earlier
             Right _ => False)
  ]

export
run : IO Bool
run = runSuite "interval tests" intervalCases
