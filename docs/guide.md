# iotaTime guide

This guide complements the [complete API index](index.html). It starts with the distinction that drives the library, then follows a value from civil time to physical time and back. Every valid code example is mirrored by `GuideExamples` or `GuidedMeeting` in the `examples/` package and compiled in CI; the deliberately invalid date is covered by the compile-fail suite.

## Why iotaTime exists

iotaTime is a proof-oriented Idris 2 port of HodaTime. It separates values that are merely *labels* from values that identify a point on the global timeline, and it moves calendar and clock invariants into types wherever Idris can prove them.

A statically known valid date needs no runtime check:

```idris
leapDay : CalendarDate Gregorian
leapDay = calendarDate 29 February 2020
```

The corresponding invalid declaration does not compile because Idris cannot produce `So False`:

```idris
invalid : CalendarDate Gregorian
invalid = calendarDate 29 February 2021
```

Values learned from text, files, the operating system, or users remain fallible. Runtime refiners return `Either Error ValidValue`, after which normal operations preserve the invariant.

## Core concepts

Almost everything follows from the difference between **physical time** and **civil time**.

| Type | Meaning |
| --- | --- |
| [`Instant`](docs/IotaTime.Instant.html) | One point on the global timeline. |
| [`Duration`](docs/IotaTime.Duration.html) | An exact elapsed amount, independent of calendars and zones. |
| [`Interval`](docs/IotaTime.Interval.html) | A proof-carrying half-open range of instants. |
| [`CalendarDate`](docs/IotaTime.Calendar.html) | A valid date in a particular calendar. |
| [`LocalTime`](docs/IotaTime.LocalTime.html) | A wall-clock time with no date or zone. |
| [`CalendarDateTime`](docs/IotaTime.CalendarDateTime.html) | A date and local time, still not tied to the timeline. |
| [`OffsetDateTime`](docs/IotaTime.OffsetDateTime.html) | Civil time anchored by one fixed UTC offset. |
| [`TimeZone`](docs/IotaTime.DateTimeZone.html) | The historical and future offset rules for a place. |
| [`ZonedDateTime`](docs/IotaTime.ZonedDateTime.html) | Civil time resolved in a zone and therefore tied to an instant. |
| [`Period`](docs/IotaTime.Period.html) | A calendar-relative amount whose type records required capabilities. |
| [`Pattern`](docs/IotaTime.Pattern.html) | One typed description used for both parsing and formatting. |

A label such as “23 April 2024 at 09:00” is civil time. It becomes physical time only after you supply an offset or zone and decide how daylight-saving gaps and overlaps are resolved.

## A meeting in two zones

This is the iotaTime version of HodaTime's opening example. It schedules a Zürich meeting, resolves it to an instant, and displays that same instant in New York.

```idris
module GuidedMeeting

import IotaTime

meetingDate : CalendarDate Gregorian
meetingDate = calendarDate 23 April 2024

meetingTime : LocalTime
meetingTime = localTime 9 0 0 0

meeting : CalendarDateTime Gregorian
meeting = at meetingDate meetingTime

main : IO ()
main = do
  zurichResult <- timeZone "Europe/Zurich"
  newYorkResult <- timeZone "America/New_York"
  case (zurichResult, newYorkResult) of
    (Right zurich, Right newYork) =>
      case fromCalendarDateTimeStrictly meeting zurich of
        Left _ => putStrLn "The meeting time is skipped or ambiguous"
        Right here =>
          case IotaTime.ZonedDateTime.fromInstant
            (IotaTime.ZonedDateTime.toInstant here) newYork of
            Left _ => putStrLn "The instant is outside the Gregorian range"
            Right there => do
              let display = zonedDateTimePattern pF
                    (\value => " " ++ zoneAbbreviation value)
              putStrLn (formatZonedDateTime display here)
              putStrLn (formatZonedDateTime display there)
    _ => putStrLn "Could not load both time zones"
```

In April, the two lines describe the same instant as 09:00 in Zürich and 03:00 in New York. The conversion is explicit:

1. `calendarDate` and `localTime` construct valid civil values.
2. `at` combines them without pretending they identify an instant.
3. `fromCalendarDateTimeStrictly` supplies Zürich's rules and refuses to guess for skipped or ambiguous local times.
4. `toInstant` crosses into physical time.
5. `fromInstant` expresses that same point using New York's zone rules.

## Physical time

An [`Instant`](docs/IotaTime.Instant.html) is independent of calendars and zones. A [`Duration`](docs/IotaTime.Duration.html) is an exact number of nanoseconds.

```idris
start : Instant
start = fromSecondsSinceUnixEpoch 0

elapsed : Duration
elapsed = IotaTime.Duration.fromMinutes 90

finish : Instant
finish = IotaTime.Instant.add start elapsed

checked : Duration
checked = difference finish start
```

`fromStandardDays 1` means exactly 24 hours. It is intentionally different from `days 1`, which is a calendar-relative [`Period`](docs/IotaTime.Period.html). Across daylight saving, “24 hours later” and “the same local time tomorrow” may be different instants.

## Civil time and periods

Date and clock components are opaque refined values. Valid literals elaborate directly; runtime integers use refiners such as `refineGregorianDate` and `refineLocalTime`.

```idris
late : LocalTime
late = localTime 23 30 0 0

endOfMonth : CalendarDateTime Gregorian
endOfMonth = at (calendarDate 31 January 2000) late

advanced : CalendarDateTime Gregorian
advanced = applyPeriod (months 1 <+> hours 2) endOfMonth
```

Periods apply date fields from largest to smallest, then time fields. Month arithmetic clamps invalid end-of-month days, and time rollover carries into the date. The target index rejects applying month fields to a `LocalTime` or hour fields to a `CalendarDate`.

## Skipped and ambiguous local times

A zone transition can make a civil label map to no instant or to multiple instants. iotaTime requires an explicit policy:

- `fromCalendarDateTimeAll` returns every valid mapping.
- `fromCalendarDateTimeStrictly` reports `DateTimeDoesNotExist` or `DateTimeAmbiguous`.
- `fromCalendarDateTimeLeniently` moves skipped times forward and selects the earlier ambiguous instant.
- `resolveLocal` exposes the complete mapping for a custom policy.

The reverse journey is always unambiguous: at every instant a zone has exactly one effective offset.

## Calendars

The calendar is part of the date type. `CalendarDate Gregorian`, `CalendarDate Julian`, and `CalendarDate HebrewCivil` cannot be mixed accidentally.

```idris
gregorianChristmas : CalendarDate Gregorian
gregorianChristmas = calendarDate 25 December 2024

julianChristmas : Either CalendarConversionError (CalendarDate Julian)
julianChristmas =
  IotaTime.Calendar.withCalendar gregorianChristmas
```

The result is an `Either` because calendars have different supported ranges. iotaTime includes Gregorian, ISO week dates, Julian, Coptic, Persian, indexed tabular Islamic calendars, and civil or scriptural Hebrew month numbering.

Calendar-specific month structure also lives in types. `HebrewMonths.AdarI` requires erased evidence that its year is leap, so selecting it for a common year is unrepresentable.

## Typed parsing and formatting

A [`Pattern`](docs/IotaTime.Pattern.html) carries a parser and formatter together. Standard patterns cover common layouts:

```idris
text : String
text = format (pR {calendar = Gregorian}) (calendarDate 3 March 2020)

parsed : Either PatternError (CalendarDate Gregorian)
parsed = parse (pR {calendar = Gregorian}) "2020-03-03"
```

Custom fields compose without a stringly-typed format language:

```idris
custom : Pattern DateFields (CalendarDate Gregorian)
custom =
  ((pyyyy {calendar = Gregorian} <% char '/') <+>
   (pMM {calendar = Gregorian} <% char '/')) <+>
  pdd {calendar = Gregorian}
```

The [calendar-date pattern reference](docs/IotaTime.Pattern.CalendarDate.html) groups year, month, day, and standard patterns. Equivalent pages cover local times, offsets, durations, instants, offset date-times, and zoned date-times.

## Locale-driven layouts

Built-in `enUS`, `deDE`, and `jaJP` values are pure. Native locale acquisition reads operating-system data at an explicit `IO` boundary.

```idris
germanDate : Either StrftimeError
  (Pattern DateFields (CalendarDate Gregorian))
germanDate = localeDatePattern deDE
```

`localeDatePattern`, `localeTimePattern`, and `localeDateTimePattern` compile OS-style `strftime` layouts into typed patterns. Locale date layouts remain Gregorian because native locale snapshots expose exactly twelve Gregorian month names.

## Where to go next

- Browse the [API index](index.html) for all modules.
- Start with [`IotaTime`](docs/IotaTime.html) when importing the umbrella API.
- Read the grouped references for [`Instant`](docs/IotaTime.Instant.html), [`Duration`](docs/IotaTime.Duration.html), [`Gregorian`](docs/IotaTime.Calendar.Gregorian.html), [`LocalTime`](docs/IotaTime.LocalTime.html), [`ZonedDateTime`](docs/IotaTime.ZonedDateTime.html), and [date patterns](docs/IotaTime.Pattern.CalendarDate.html).
- See the repository README for platform setup, native timezone details, and complete calendar-specific examples.
