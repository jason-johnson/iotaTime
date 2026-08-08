## Using CalendarDateTime

### Combine a date and local time

A `CalendarDateTime` is still civil time: it does not identify an instant until
an offset or time zone resolves it.

```idris
endOfMonth : CalendarDateTime Gregorian
endOfMonth = at (IotaTime.Calendar.Gregorian.calendarDate 31 January 2000) late
```

### Apply calendar and clock units

Periods apply larger calendar units first and clamp invalid month-end dates;
time overflow then carries into the date.

```idris
advanced : CalendarDateTime Gregorian
advanced = applyPeriod (months 1 <+> hours 2) endOfMonth
```

These declarations are compiled from `examples/GuideExamples.idr`.
