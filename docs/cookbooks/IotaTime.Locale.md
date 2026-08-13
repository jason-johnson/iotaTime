## Cookbook

### Use a deterministic built-in locale

Use `enUS`, `deDE`, or `jaJP` when output must not depend on locales installed on the host. `localeId` and the name-table accessors let applications inspect the selected locale without exposing its representation.

```idris
germanLocale : Locale
germanLocale = deDE

germanLocaleId : String
germanLocaleId = localeId germanLocale
```

Built-in locales also supply the layouts used by locale-aware patterns.

```idris
germanDate : Either StrftimeError
  (Pattern DateFields (CalendarDate Gregorian))
germanDate = localeDatePattern deDE
```

See [`IotaTime.Pattern`](IotaTime.Pattern.html) for pattern composition and
[`IotaTime.Pattern.Locale`](IotaTime.Pattern.Locale.html) for the date, time,
date-time, offset, and zoned layouts that consume `Locale` values.

### Load the user's current locale

`currentLocale` reads the operating-system locale selection. On Unix, `LC_ALL`, `LC_TIME`, and `LANG` determine its identifier; Windows uses the current user locale.

```idris
configuredLocale : IO (Either LocaleError Locale)
configuredLocale = currentLocale
```

Keep the `Either` boundary because the configured locale or native locale service may be unavailable.

### Load a named operating-system locale

Use `localeByName` when a locale identifier comes from configuration. Identifiers are platform-specific; the POSIX `C` locale is a useful Unix baseline.

```idris
posixLocale : IO (Either LocaleError Locale)
posixLocale = localeByName "C"
```

A missing identifier returns `Left (LocaleNotFound name)` rather than falling back silently.

These declarations are compiled from `examples/GuideExamples.idr`.
