## Cookbook

### Format and parse with a standard pattern

A `Pattern` handles both directions. The round-trip calendar-date pattern formats a value and validates the whole input when parsing.

```idris
roundTripText : String
roundTripText = format (pR {calendar = Gregorian})
  (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020)

roundTripDate : Either PatternError (CalendarDate Gregorian)
roundTripDate = parse (pR {calendar = Gregorian}) "2020-03-03"
```

### Build a custom pattern

Join compatible field patterns with `<+>` and append fixed text with `<%`. This pattern uses slashes between the year, month, and day.

```idris
customDatePattern : Pattern DateFields (CalendarDate Gregorian)
customDatePattern =
  ((pyyyy {calendar = Gregorian} <% char '/') <+>
   (pMM {calendar = Gregorian} <% char '/')) <+>
  pdd {calendar = Gregorian}
```

### Mark a scalar with protocol text

Literals can make an application-selected representation self-describing without defining a library-wide interchange format.

```idris
signedIntegerWire : Pattern Integer Integer
signedIntegerWire = pSignedInteger <% string "i"
```

These declarations are compiled from `examples/GuideExamples.idr`.