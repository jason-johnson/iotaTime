## Cookbook

### Construct a Coptic date

Coptic months have their own namespace. The first twelve contain 30 days; `PiKogiEnavot` is the short epagomenal month at the end of the year.

```idris
copticNewYear : CalendarDate Coptic
copticNewYear = IotaTime.Calendar.Coptic.calendarDate
  1 CopticMonths.Thout 1738
```

### Validate an epagomenal day

The sixth day of `PiKogiEnavot` exists only in a Coptic leap year. Use `refineDate` when the year or day comes from input.

```idris
copticEpagomenalInput : Either CopticDateError (CalendarDate Coptic)
copticEpagomenalInput = IotaTime.Calendar.Coptic.refineDate
  6 CopticMonths.PiKogiEnavot 1732
```

Because 1732 is not a leap year, this value is `Left` with a typed date error.

These declarations are compiled from `examples/GuideExamples.idr`.
