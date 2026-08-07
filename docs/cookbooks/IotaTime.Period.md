## Using Period

### Combine calendar and clock units

A `Period target` records which units its target supports. A
`CalendarDateTime` accepts both calendar-relative months and clock-relative
hours.

```idris
calendarAndClockPeriod : Period (CalendarDateTime Gregorian)
calendarAndClockPeriod = months 1 <+> hours 2
```

The target index rejects nonsensical combinations at compile time, such as
months for `LocalTime` or hours for `CalendarDate`.

### Apply a period

`applyPeriod` follows the target's calendar and rollover rules rather than
pretending calendar units are fixed durations.

```idris
advanced : CalendarDateTime Gregorian
advanced = applyPeriod calendarAndClockPeriod endOfMonth
```

These declarations are compiled from `examples/GuideExamples.idr`.
