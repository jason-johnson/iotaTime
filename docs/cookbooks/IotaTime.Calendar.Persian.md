## Cookbook

### Choose the astronomical or arithmetic calendar

`Persian` is the official astronomical calendar over its vouched years 1-1500. `PersianArithmetic` uses Birashk's exact 2820-year rule through year 9377. Their types prevent accidental mixing, including in years where their dates diverge.

```idris
astronomicalNowruz1404 : CalendarDate Persian
astronomicalNowruz1404 = persianDate 1 PersianMonths.Farvardin 1404

arithmeticNowruz1404 : CalendarDate PersianArithmetic
arithmeticNowruz1404 =
  arithmeticPersianDate 1 PersianMonths.Farvardin 1404
```

The astronomical date is Gregorian 21 March 2025; the arithmetic date is 20 March 2025.

### Validate a legacy Simple-cycle date

`PersianSimple` reproduces the 33-year Persian rule used by the BCL before .NET 4.6. Runtime components cross the same typed refinement boundary as other calendars.

```idris
simplePersianRuntime : Either PersianDateError (CalendarDate PersianSimple)
simplePersianRuntime =
  refineSimplePersianDate 30 PersianMonths.Esfand 1404
```

These declarations are compiled from `examples/GuideExamples.idr`.