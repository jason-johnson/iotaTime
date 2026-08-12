## Using TimeZone

`IotaTime.TimeZone` publicly re-exports the opaque zone type and its error
types, but no zone operations. Acquire system and named zones through
[`IotaTime.Tzdb`](IotaTime.Tzdb.html), and resolve local civil times or work
with zoned values through
[`IotaTime.ZonedDateTime`](IotaTime.ZonedDateTime.html). Lower-level
declarations in this module use `export` for direct internal consumers and are
not part of the re-exported public API documented here.

### Load a named system time zone

Zone lookup is an IO operation because it reads the platform's time-zone data.
Callers handle missing or malformed zone data through `TzdbError`.

```idris
systemZurich : IO (Either TzdbError TimeZone)
systemZurich = timeZone "Europe/Zurich"
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
