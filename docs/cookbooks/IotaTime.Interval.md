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
