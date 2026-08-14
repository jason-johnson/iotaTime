## Cookbook

### Choose the astronomical or civil epoch

`IslamicBcl` and `CivilIslamicBcl` use the same Base16 leap-year pattern but different epochs. Their distinct types prevent dates from the two calendars being mixed accidentally.

```idris
islamicNewYear : CalendarDate IslamicBcl
islamicNewYear = IotaTime.Calendar.Islamic.calendarDate
  1 IslamicMonths.Muharram 1443

civilIslamicNewYear : CalendarDate CivilIslamicBcl
civilIslamicNewYear = IotaTime.Calendar.Islamic.civilCalendarDate
  1 IslamicMonths.Muharram 1443
```

The civil date is one absolute day later than the astronomical date.

### Select a leap-year pattern explicitly

The convenience constructors use the Base16 pattern compatible with BCL. Use the primed constructors when another tabular pattern is part of the calendar definition.

```idris
base15IslamicNewYear : CalendarDate IslamicBase15
base15IslamicNewYear = IotaTime.Calendar.Islamic.calendarDate'
  {pattern = Base15} 1 IslamicMonths.Muharram 1443
```

The result type records the selected pattern.

These declarations are compiled from `examples/GuideExamples.idr`.
