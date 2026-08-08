## Using CalendarDate

### Construct a statically known date

`IotaTime.Calendar.Gregorian.calendarDate` constructs a Gregorian date whose validity Idris proves while
elaborating. Invalid literals, such as 29 February 2021, do not compile.

```idris
leapDay : CalendarDate Gregorian
leapDay = IotaTime.Calendar.Gregorian.calendarDate 29 February 2020
```

### Validate date components at runtime

Use the calendar-specific refiner when components come from input. It returns a
typed error instead of admitting an invalid date.

```idris
runtimeLeapDay : Either GregorianDateError (CalendarDate Gregorian)
runtimeLeapDay = IotaTime.Calendar.Gregorian.refineDate 29 February 2020
```

### Convert between calendars

`withCalendar` preserves the absolute day while changing its calendar
representation. The result is fallible because the target calendar may not
cover that day.

```idris
gregorianChristmas : CalendarDate Gregorian
gregorianChristmas = IotaTime.Calendar.Gregorian.calendarDate 25 December 2024

julianChristmas : Either CalendarConversionError (CalendarDate Julian)
julianChristmas = IotaTime.Calendar.withCalendar gregorianChristmas
```

### Decompose a difference by calendar units

`between` applies the standard largest-first, non-overshooting policy. Each
accepted component is applied before the next smaller unit is calculated.

```idris
differenceStart : CalendarDate Gregorian
differenceStart = IotaTime.Calendar.Gregorian.calendarDate 31 January 2025

differenceEnd : CalendarDate Gregorian
differenceEnd = IotaTime.Calendar.Gregorian.calendarDate 30 March 2025

calendarDifference : Period (CalendarDate Gregorian)
calendarDifference = IotaTime.Calendar.between differenceStart differenceEnd
```

The concrete date representation determines its calendar, so callers do not
repeat `{calendar = Gregorian}`. Calendar-polymorphic implementation code that
has a `Calendar calendar` dictionary rather than a concrete representation can
use `betweenFor {calendar}`, `betweenDaysFor {calendar}`, or
`betweenWithFor {calendar}`.

This period contains one month and 30 days. A direct two-month application
would produce 31 March, which passes the endpoint.

### Request an exact day difference

Use `betweenDays` when the protocol or calculation needs only elapsed civil
days. The equivalent explicit policy is available through `betweenWith`.

```idris
exactDayDifference : Period (CalendarDate Gregorian)
exactDayDifference = IotaTime.Calendar.betweenDays differenceStart differenceEnd

explicitDayDifference : Period (CalendarDate Gregorian)
explicitDayDifference = IotaTime.Calendar.betweenWith
  (MkDateDifferencePolicy DaysOnly ClampToMonth)
  differenceStart differenceEnd
```

Both exact forms contain 58 days. These declarations are compiled from
`examples/GuideExamples.idr`.