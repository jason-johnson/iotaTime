## Cookbook

### Construct or validate a Gregorian date

Use `calendarDate` when the components are known while Idris elaborates the program. Use `refineDate` for runtime input so an invalid leap day or a date outside the supported historical range produces a typed error.

```idris
leapDay : CalendarDate Gregorian
leapDay = IotaTime.Calendar.Gregorian.calendarDate 29 February 2020

runtimeLeapDay : Either GregorianDateError (CalendarDate Gregorian)
runtimeLeapDay = IotaTime.Calendar.Gregorian.refineDate 29 February 2020
```

### Handle the Gregorian cutover

The Gregorian calendar begins on 15 October 1582. Runtime refinement rejects dates in the omitted part of October instead of silently treating the calendar as proleptic.

```idris
gregorianCutoverInput : Either GregorianDateError (CalendarDate Gregorian)
gregorianCutoverInput = IotaTime.Calendar.Gregorian.refineDate
  14 October 1582
```

This value is `Left (InvalidGregorianDate 14 October 1582)`.

These declarations are compiled from `examples/GuideExamples.idr`.
