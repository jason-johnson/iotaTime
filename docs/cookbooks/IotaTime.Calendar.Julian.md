## Cookbook

### Apply Julian leap-year rules

The Julian calendar treats every year divisible by four as a leap year. Consequently, 29 February 1900 is a valid Julian date even though it is not a valid Gregorian date.

```idris
julianLeapDay1900 : CalendarDate Julian
julianLeapDay1900 = IotaTime.Calendar.Julian.calendarDate
  29 JulianMonths.February 1900
```

### Convert a Julian date to Gregorian

`withCalendar` preserves the absolute day and changes its calendar representation. Conversion is fallible because the target calendar may not cover the source day.

```idris
julianLeapDayAsGregorian : Either CalendarConversionError
  (CalendarDate Gregorian)
julianLeapDayAsGregorian = withCalendar julianLeapDay1900
```

These declarations are compiled from `examples/GuideExamples.idr`.
