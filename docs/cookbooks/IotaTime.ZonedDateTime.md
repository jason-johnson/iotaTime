## Using ZonedDateTime

### Display an instant in a zone

Resolving from an instant is unambiguous: the zone's transition history selects
the effective offset and local fields.

```idris
zonedEpoch : TimeZone -> Either CalendarConversionError
  (ZonedDateTime Gregorian)
zonedEpoch zone = IotaTime.ZonedDateTime.fromInstant start zone
```

### Preserve the instant across zones

Convert a zoned value to `Instant`, then display that instant in another zone.
The local fields change while timeline identity is preserved.

```idris
sameInstantIn : TimeZone -> ZonedDateTime Gregorian ->
  Either CalendarConversionError (ZonedDateTime Gregorian)
sameInstantIn zone value =
  IotaTime.ZonedDateTime.fromInstant
    (IotaTime.ZonedDateTime.toInstant value) zone
```

These declarations are compiled from `examples/GuideExamples.idr`.
