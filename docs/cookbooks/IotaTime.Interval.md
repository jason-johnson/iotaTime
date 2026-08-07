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

These declarations are compiled from `examples/GuideExamples.idr`.
