## Cookbook

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
or supply `MkTimeZoneCachePolicy` to select each cache independently.

These declarations are compiled from `examples/GuideExamples.idr`.