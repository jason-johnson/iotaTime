## Using LocalTime

### Construct a known wall-clock time

A `LocalTime` is a time of day without a date, offset, or zone. Literal
components are checked during elaboration.

```idris
late : LocalTime
late = localTime 23 30 0 0
```

### Validate runtime components

Use `refineLocalTime` for components learned at runtime. Its typed error says
which component was invalid.

```idris
runtimeTime : Either LocalTimeError LocalTime
runtimeTime = refineLocalTime 9 30 0 0
```

These declarations are compiled from `examples/GuideExamples.idr`.
