## Cookbook

### Format a quoted zone identifier

The standard quoted pattern combines the ISO local date and time with an escaped zone ID. It can represent both IANA IDs and Windows names containing spaces.

```idris
quotedZonedPattern :
  ZonedDateTimePattern (DateFields, TimeFields) (ZonedDateTime Gregorian)
quotedZonedPattern = zonedDateTimePattern {calendar = Gregorian} ps
  (\value => " " ++ format pZoneIdQuoted (zoneId value))
```

For this layout, `pZonedDateTimeQuoted` is the ready-made equivalent.

### Parse with an application-selected zone syntax

Parsing a zoned value also requires a zone provider and a policy for skipped or ambiguous local times. `parseZonedDateTimePatternWith` keeps those choices explicit while allowing the protocol to select quoted zone IDs.

```idris
parseQuotedWindowsZone :
  (String -> IO (Either error TimeZone)) ->
  (CalendarDateTime Gregorian -> TimeZone ->
    Either resolutionError (ZonedDateTime Gregorian)) ->
  IO (Either (ZonedDateTimePatternError error resolutionError)
    (ZonedDateTime Gregorian))
parseQuotedWindowsZone provider resolver =
  parseZonedDateTimePatternWith {calendar = Gregorian}
    ps pZoneIdQuoted provider resolver
    "1970-01-01T00:00:00 \"Eastern Standard Time\""
```

These declarations are compiled from `examples/GuideExamples.idr`.