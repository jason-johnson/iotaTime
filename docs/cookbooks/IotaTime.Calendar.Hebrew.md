## Cookbook

### Choose civil or scriptural month numbering

`HebrewCivil` numbers the year from Tishri, while `HebrewScriptural` numbers it from Nisan. The month constructors describe the same named months, but the distinct calendar types preserve the numbering convention.

```idris
hebrewPassover : CalendarDate HebrewCivil
hebrewPassover = IotaTime.Calendar.Hebrew.calendarDate
  15 5784 HebrewMonths.Nisan

scripturalHebrewPassover : CalendarDate HebrewScriptural
scripturalHebrewPassover = IotaTime.Calendar.Hebrew.calendarDate'
  {numbering = Scriptural} 15 5784 HebrewMonths.Nisan
```

### Validate a leap-only month from input

`AdarI` exists only in leap years. Static month construction requires proof of that fact; runtime code can instead pass `AdarIName` to `refineDate` and receive a typed failure when the month is absent.

```idris
hebrewLeapMonthInput : Either HebrewDateError (CalendarDate HebrewCivil)
hebrewLeapMonthInput = IotaTime.Calendar.Hebrew.refineDate
  1 AdarIName 5786
```

Because 5786 is a common year, this value is
`Left (InvalidHebrewMonth AdarIName 5786)`.

These declarations are compiled from `examples/GuideExamples.idr`.
