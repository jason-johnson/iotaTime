## Using OffsetDateTime

### Anchor civil time with a fixed offset

An `OffsetDateTime` pairs civil fields with one fixed UTC offset, giving the
value a unique corresponding instant.

```idris
fixedOffsetDateTime : OffsetDateTime Gregorian
fixedOffsetDateTime = fromCalendarDateTimeWithOffset
  endOfMonth centralEuropeanOffset

fixedOffsetInstant : Instant
fixedOffsetInstant = IotaTime.OffsetDateTime.toInstant fixedOffsetDateTime
```

### Change the displayed offset

`withOffset` preserves the instant while changing the local date and time used
to display it.

```idris
displayAtUtc : Either CalendarConversionError (OffsetDateTime Gregorian)
displayAtUtc = IotaTime.OffsetDateTime.withOffset
  (IotaTime.Offset.fromHours 0) fixedOffsetDateTime
```

These declarations are compiled from `examples/GuideExamples.idr`.
