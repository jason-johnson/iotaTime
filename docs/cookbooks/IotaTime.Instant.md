## Using Instant

### Start from the Unix epoch

An `Instant` identifies one point on the global timeline without a calendar,
offset, or time zone.

```idris
start : Instant
start = fromSecondsSinceUnixEpoch 0
```

### Move by exact elapsed time

Add a `Duration` to move along the timeline. Subtract two instants with
`difference` to recover the exact elapsed duration.

```idris
finish : Instant
finish = IotaTime.Instant.add start elapsed

checked : Duration
checked = difference finish start
```

These declarations are compiled from `examples/GuideExamples.idr`.
