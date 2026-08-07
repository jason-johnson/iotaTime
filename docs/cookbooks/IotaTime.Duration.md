## Using Duration

### Construct an exact elapsed duration

A `Duration` is a fixed amount of timeline time. Minutes, hours, and standard
days always mean 60 seconds, 60 minutes, and 24 hours; they do not follow
calendar or daylight-saving transitions.

```idris
elapsed : Duration
elapsed = IotaTime.Duration.fromMinutes 90
```

### Combine durations

Duration arithmetic remains exact and exposes the underlying signed nanosecond
count when a protocol or calculation needs it.

```idris
combinedDuration : Duration
combinedDuration = IotaTime.Duration.add
  (IotaTime.Duration.fromHours 1)
  (IotaTime.Duration.fromMinutes 30)
```

These declarations are compiled from `examples/GuideExamples.idr`.
