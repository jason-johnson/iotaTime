## Cookbook

### Decompose a difference by calendar units

`between` applies the standard largest-first, non-overshooting policy. Each
accepted component is applied before the next smaller unit is calculated.

```idris
calendarDifference : Period (CalendarDate Gregorian)
calendarDifference = IotaTime.Calendar.between {calendar = Gregorian}
  (calendarDate 31 January 2025) (calendarDate 30 March 2025)
```

This period contains one month and 30 days. A direct two-month application
would produce 31 March, which passes the endpoint.

### Request an exact day difference

Use `betweenDays` when the protocol or calculation needs only elapsed civil
days. The equivalent explicit policy is available through `betweenWith`.

```idris
exactDayDifference : Period (CalendarDate Gregorian)
exactDayDifference = IotaTime.Calendar.betweenDays {calendar = Gregorian}
  (calendarDate 31 January 2025) (calendarDate 30 March 2025)

explicitDayDifference : Period (CalendarDate Gregorian)
explicitDayDifference = IotaTime.Calendar.betweenWith {calendar = Gregorian}
  (MkDateDifferencePolicy DaysOnly ClampToMonth)
  (calendarDate 31 January 2025) (calendarDate 30 March 2025)
```

Both exact forms contain 58 days. These declarations are compiled from
`examples/GuideExamples.idr`.