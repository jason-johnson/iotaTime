## Using Clock

### Read the system clock

Production code can depend on the `Clock` interface and use `systemClock` to
obtain the host's current instant.

```idris
current : IO Instant
current = getCurrentInstant systemClock
```

### Make time deterministic in tests

A `FixedClock` always returns its supplied instant, so tests need no sleeps or
wall-clock assumptions.

```idris
deterministicClock : FixedClock
deterministicClock = fixedClock start

readDeterministicClock : IO Instant
readDeterministicClock = getCurrentInstant deterministicClock
```

These declarations are compiled from `examples/GuideExamples.idr`.
