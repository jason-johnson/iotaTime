## Cookbook

### Use one Windows registry snapshot

Use a snapshot provider when a process should share one consistent registry
view across named lookup, local lookup, and enumeration. Construct another
provider to observe later registry changes.

```idris
windowsSnapshotProvider : IO (Either TzdbError TimeZoneProvider)
windowsSnapshotProvider = windowsSnapshotTimeZoneProvider
```

These declarations are compiled from `examples/GuideExamples.idr`.