## Cookbook

### Use the operating system's locale layout

`Locale` is opaque: applications can obtain one from the operating system, but cannot inject an arbitrary `strftime` layout. The locale pattern functions compile only the trusted layout captured by `currentLocale` or `localeByName`.

```idris
configuredDatePattern : IO (Either LocaleError
  (Either StrftimeError (Pattern DateFields (CalendarDate Gregorian))))
configuredDatePattern = do
  loaded <- currentLocale
  pure (map localeDatePattern loaded)
```

Keep both `Either` layers. The outer layer reports locale acquisition failures; the inner layer reports a native layout that uses a conversion iotaTime does not support.

### Use a deterministic built-in locale

Built-in locales use the same opaque path when output must be stable across machines.

```idris
germanDate : Either StrftimeError
  (Pattern DateFields (CalendarDate Gregorian))
germanDate = localeDatePattern deDE
```

### Parse a localized zoned date-time

`parseZonedDateTime` uses the date-time layout hidden inside the locale. Zone loading and local-time resolution remain explicit application policies.

```idris
parseLocalizedZoned :
  (String -> IO (Either providerError TimeZone)) ->
  (CalendarDateTime Gregorian -> TimeZone ->
    Either resolutionError (ZonedDateTime Gregorian)) ->
  IO (Either (ZonedPatternError providerError resolutionError)
    (ZonedDateTime Gregorian))
parseLocalizedZoned provider resolver =
  parseZonedDateTime provider resolver enUS
    "Sun 15 Mar 2020 01:24:35 PM UTC"
```

Arbitrary string-to-pattern compiler functions are intentionally not part of the public API.

These declarations are compiled from `examples/GuideExamples.idr`.
