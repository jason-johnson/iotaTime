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

emptyUnbounded : UnboundedInterval
emptyUnbounded = unboundedInterval (Just 0) (Just 0)

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
    , MkRuntimeCase "intersection returns the shared non-empty range"
      (intersection leftInterval overlappingInterval == Just (interval 5 10))
    , MkRuntimeCase "adjacent intervals have no non-empty intersection"
      (intersection leftInterval adjacentInterval == Nothing)
    , MkRuntimeCase "connected union spans overlapping intervals"
      (IotaTime.Interval.union leftInterval overlappingInterval ==
      Just (interval 0 15))
    , MkRuntimeCase "connected union spans adjacent intervals"
      (IotaTime.Interval.union leftInterval adjacentInterval ==
      Just (interval 0 20))
    , MkRuntimeCase "connected union rejects separated intervals"
      (IotaTime.Interval.union leftInterval separateInterval == Nothing)
    , MkRuntimeCase "connected union absorbs an empty interval"
      (IotaTime.Interval.union emptyInterval leftInterval == Just leftInterval)
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
  , MkRuntimeCase "unbounded intersection selects the tighter bounds"
      (unboundedIntersection fromZero untilTen == Just finiteUnbounded)
  , MkRuntimeCase "unbounded union extends through infinite bounds"
      (unboundedUnion fromZero untilTen == Just allTime)
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
        in unboundedIntersection untilZero afterOne == Nothing &&
          unboundedUnion untilZero afterOne == Nothing)
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
