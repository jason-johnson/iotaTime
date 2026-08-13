## Using TimeZone

`IotaTime.TimeZone` re-exports the opaque `TimeZone` type and provides the
HodaTime-compatible platform loaders, explicit providers, caching, and
metadata.
Resolve local civil times and work with zoned values through
[`IotaTime.ZonedDateTime`](IotaTime.ZonedDateTime.html).

### Load a named system time zone

Zone lookup is an IO operation because it reads the platform's time-zone data.
Callers handle missing or malformed zone data through `TzdbError`.

```idris
systemZurich : IO (Either TzdbError TimeZone)
systemZurich = timeZone "Europe/Zurich"
```

### Cache successful provider queries

Caching is opt-in and owned by the returned provider. The default policy
retains named zones, enumeration, and metadata while continuing to read the
host's local-zone configuration on every query.

```idris
cachedProvider : IO TimeZoneProvider
cachedProvider = cachedTimeZoneProvider
	defaultTimeZoneCachePolicy systemTimeZoneProvider

cachedZurich : IO (Either TzdbError TimeZone)
cachedZurich = do
	provider <- cachedProvider
	timeZoneWith provider "Europe/Zurich"
```

Failures are not cached. Construct a new wrapper to start with empty caches,
or use `timeZoneCachePolicy` to select each cache independently.

### Use one Windows registry snapshot

Use a snapshot provider when a process should share one consistent registry
view across named lookup, local lookup, and enumeration. Construct another
provider to observe later registry changes.

```idris
windowsSnapshotProvider : IO (Either TzdbError TimeZoneProvider)
windowsSnapshotProvider = windowsSnapshotTimeZoneProvider
```

### Resolve local civil time deliberately

Local times near transitions can be skipped or ambiguous. Use the strict, all,
or lenient mapping API according to application policy; do not infer an offset
from the zone ID alone.

```idris
resolveEndOfMonth : TimeZone -> Either ZonedDateTimeError
	(ZonedDateTime Gregorian)
resolveEndOfMonth zone = fromCalendarDateTimeStrictly endOfMonth zone
```

These declarations are compiled from `examples/GuideExamples.idr`.
