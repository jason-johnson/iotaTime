## Using Interval

### Construct a known half-open interval

`Interval` represents `[start, end)`: it contains the start but not the end.
Literal nanosecond endpoints are checked by Idris while elaborating.

```idris
boundedWindow : Interval
boundedWindow = interval 0 5400000000000

windowContainsStart : Bool
windowContainsStart = contains boundedWindow start
```

### Validate runtime endpoints

Use `refineInterval` when endpoints come from input. Reversed endpoints produce
`IntervalError` instead of an invalid interval.

```idris
runtimeWindow : Either IntervalError Interval
runtimeWindow = refineInterval start finish
```

### Intersect or unite intervals

`intersection` and `union` return `Interval` directly when the caller already
carries erased evidence for the required relationship. This is useful in
proof-directed code without introducing a failure branch.

```idris
proofDirectedIntersection : (left, right : Interval) ->
	{auto 0 intersects : So (hasNonEmptyIntersection left right)} -> Interval
proofDirectedIntersection = intersection

proofDirectedUnion : (left, right : Interval) ->
	{auto 0 connected : So (isConnected left right)} ->
	Interval
proofDirectedUnion = IotaTime.Interval.union
```

Because `Interval` is opaque, Idris does not reconstruct relationships between
previously constructed values from their original literals. Use the typed
runtime refiners when evidence is not already in scope:

```idris
runtimeIntersection : Either IntersectionError Interval
runtimeIntersection = refineIntersection
	(interval 0 10) (interval 5 15)

runtimeUnion : Either UnionError Interval
runtimeUnion = refineUnion (interval 0 10) (interval 11 20)
```

`refineIntersection` returns `NoNonEmptyIntersection` for empty, adjacent, or
separated inputs. `refineUnion` returns `DisconnectedIntervals` when the inputs
cannot form one connected interval. The unbounded operations follow the same
split through `unboundedIntersection`, `refineUnboundedIntersection`,
`unboundedUnion`, and `refineUnboundedUnion`.

### Carry validity through unbounded intervals

`UnboundedInterval` uses `Nothing` for an infinite endpoint and stores erased
evidence that any two finite endpoints are ordered. Static optional nanosecond
bounds use `unboundedInterval`; runtime `Instant` bounds use
`refineUnboundedInterval`.

```idris
futureWindow : UnboundedInterval
futureWindow = unboundedInterval (Just 0) Nothing

0 futureWindowIsValid : So (isValidUnboundedInterval
	(unboundedStart GuideExamples.futureWindow)
	(unboundedEnd GuideExamples.futureWindow))
futureWindowIsValid = unboundedIntervalIsValid futureWindow
```

`toBoundedInterval` reuses this evidence when both endpoints are finite rather
than validating their order again.

These declarations are compiled from `examples/GuideExamples.idr`.
