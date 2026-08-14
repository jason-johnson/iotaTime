## Cookbook

### Construct a standards-valid ISO week date

ISO weeks begin on Monday, and week 1 contains January 4. `fromWeekDate`
requires erased evidence that the week belongs to the requested ISO year.
Years have either 52 or 53 weeks.

```idris
isoWeekFiftyThree : CalendarDate Gregorian
isoWeekFiftyThree = IotaTime.Calendar.Iso.fromWeekDate 53 Sunday 2020
```

An invalid literal such as week 53 of 2021 fails elaboration. Runtime values
use `refineWeekDate`:

```idris
runtimeIsoWeek : Either IsoWeekDateError (CalendarDate Gregorian)
runtimeIsoWeek = IotaTime.Calendar.Iso.refineWeekDate 53 Monday 2021
```

### Use unrestricted week arithmetic explicitly

Week zero, negative weeks, and weeks beyond an ISO year's actual count are not
ISO week dates. Protocols that intentionally use those coordinates can opt
into the separately named arithmetic refiner.

```idris
arithmeticIsoWeekZero : Either IsoWeekDateError (CalendarDate Gregorian)
arithmeticIsoWeekZero =
  IotaTime.Calendar.Iso.refineArithmeticWeekDate 0 Monday 2000
```

Arithmetic refinement still rejects coordinates whose resulting Gregorian day
is outside the supported range.

These declarations are compiled from `examples/GuideExamples.idr`.