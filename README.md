# iotaTime

An Idris 2 time library based on Erik Naggum's "Long painful history of time".

## Design goal: make invalid time values unrepresentable

iotaTime is a port of HodaTime to Idris 2, but reproducing the Haskell API is only an intermediate step. The project exists to use dependent types to move calendar and time invariants out of runtime validation and into the type system.

The final public API should follow these principles:

- Domain values carry the proofs needed to establish their validity. A `CalendarDate Gregorian` must always be a valid Gregorian date; there must be no second, possibly-invalid form of the same public type.
- Invalid statically known expressions must fail to compile. For example, `calendarDate 29 February 2020` should type-check while `calendarDate 29 February 2021` should not.
- Calendar-specific structure belongs in types. In calendars such as Hebrew, a month that exists only in leap years should be impossible to select for a non-leap year.
- Once a value has crossed a trust boundary, ordinary library operations should preserve its invariants and should not return `Maybe` merely to report states that the types already exclude.
- Raw representations and unchecked constructors are implementation details. Any public escape hatch needed for interoperability must be clearly distinguished from the proof-carrying domain API.
- External data remains genuinely fallible. A parser for a file, network response, database value, or user input must either produce a proven-valid value or return a descriptive error. Failure should be confined to that untrusted boundary, preferably as `Either Error ValidValue`, rather than propagated through normal calendar operations.
- Runtime refinement should use decidable propositions and erased proofs internally. The project must not establish invariants with exceptions, unchecked casts, `believe_me`, or equivalent assertions.
- Compile-time-known embedded data should be checked during compilation whenever practical, eliminating runtime failure for that data entirely.

The target is therefore not literally the absence of every failure type. It is the absence of invalid domain values and unnecessary partiality: compile-time rejection when inputs are statically known, explicit errors where information first enters the program, and total proof-preserving functions everywhere after that boundary.

## Development container / Codespaces

This repository is configured for GitHub Codespaces and VS Code Dev Containers via `.devcontainer/`.

- Uses an official Microsoft devcontainer base image (`mcr.microsoft.com/devcontainers/base:ubuntu-24.04`)
- Builds the Idris 2 0.8.0 release and installs the standard libraries and compiler API with sources
- Includes a compatible, pinned `idris2-lsp`, the `pack` package manager, and the VS Code LSP client
- Includes Chez Scheme, C and JavaScript backends, `rlwrap`, Git/Git LFS/GitHub CLI, debugging and profiling tools, and common shell utilities
- Runs entirely as the non-root `vscode` user after the image is built; no post-create tool installation is required

The Idris package collection is pinned to `nightly-251031`, the snapshot made from the Idris 2 0.8.0 release. Rebuild the container after changing any toolchain files under `.devcontainer/`.

## Project layout

- `iotaTime.ipkg` — Idris 2 library package definition
- `src/IotaTime.idr` — library entry module
- `src/IotaTime/Instant.idr` — opaque points on the global nanosecond timeline
- `src/IotaTime/Duration.idr` — opaque fixed elapsed-time amounts
- `src/IotaTime/DateTimeZone.idr` — validated fixed and transition-based zones
- `src/IotaTime/Interval.idr` — proof-carrying half-open timeline intervals
- `src/IotaTime/Offset.idr` — bounded signed UTC offsets
- `src/IotaTime/OffsetDateTime.idr` — calendar-local date-times resolved by UTC offset
- `src/IotaTime/Period.idr` — target-indexed calendar-relative periods
- `src/IotaTime/Time/Component.idr` — opaque, range-checked clock components
- `src/IotaTime/LocalTime.idr` — proof-carrying local time of day
- `src/IotaTime/CalendarDateTime.idr` — calendar date paired with local time
- `src/IotaTime/Calendar/Component.idr` — opaque and refined calendar component types
- `src/IotaTime/Calendar/Gregorian.idr` — proof-carrying Gregorian calendar
- `src/IotaTime/Calendar/Julian.idr` — proof-carrying Julian calendar
- `test/iotaTime-test.ipkg` — test package
- `test/Main.idr` — test orchestrator
- `test/Test/Proof.idr` — compile-time proof tests
- `test/Test/Runtime.idr` — runtime behavior tests
- `test/Test/Regression.idr` — regression tests
- `test/Test/Support.idr` — runtime assertions and suite reporting
- `.github/workflows/ci.yml` — CI build+test workflow for push and pull requests

## Build and test

```bash
idris2 --build iotaTime.ipkg
idris2 --install iotaTime.ipkg
idris2 --build test/iotaTime-test.ipkg
./test/build/exec/iotaTime-test
sh test/run-compile-fail-tests.sh
```

## Instants and fixed durations

`Instant` is an opaque point on the global timeline, stored canonically as an arbitrary-precision nanosecond count relative to March 1, 2000 UTC. `Duration` is a distinct opaque fixed amount measured in nanoseconds. Neither representation can overflow or requires carry normalization.

```idris
start : Instant
start = fromSecondsSinceUnixEpoch 0

finish : Instant
finish = addDuration start (durationFromMinutes 90)

elapsed : Duration
elapsed = difference finish start
```

`durationFromStandardDays` and `durationFromStandardWeeks` mean exact 24-hour and seven-day amounts. Calendar-relative `days`, `weeks`, `months`, and `years` continue to construct target-indexed `Period` values instead. `now` reads the mandatory UTC system clock, and Unix conversion is available at whole-second and nanosecond precision.

The scalar representation is intentionally simple. A proof-oriented representation using an `Integer` day, `Fin 86400` second-of-day, and `Fin 1000000000` nanosecond remains a future benchmarking candidate if profiling shows a material benefit.

## Intervals

`Interval` represents a half-open range `[start, end)` and always satisfies `start <= end`. Statically known epoch-relative nanosecond endpoints use `interval`; reversed literals fail compilation:

```idris
window : Interval
window = interval 0 1000000000
```

For arbitrary `Instant` endpoints learned at runtime, `refineInterval` returns `Either IntervalError Interval`. Empty intervals are valid, contain no instants, and have zero duration. `contains` includes the start and excludes the end; `duration` returns the fixed nonnegative `Duration` between the endpoints.

Static construction accepts scalar endpoints because `Instant` is intentionally opaque: Idris cannot reduce two arbitrary `Instant` values to synthesize their ordering proof outside the implementation module. Runtime refinement preserves that opacity without casts or unchecked constructors.

## Offsets

`Offset` is an opaque signed whole-second displacement from UTC, bounded inclusively to plus or minus 18 hours. Statically known values use unit-specific proof-carrying constructors; out-of-range literals fail compilation:

```idris
india : Offset
india = offsetFromMinutes 330

minimumOffset : Offset
minimumOffset = offsetFromHours (-18)
```

`refineOffsetSeconds` validates an arbitrary runtime total and returns `Either OffsetError Offset`. `totalOffsetSeconds` exposes the scalar value. `offsetHours`, `offsetMinutes`, and `offsetSeconds` are sign-consistent components, so `-01:30:45` yields `(-1, -30, -45)` and the components always reconstruct the total.

`addOffsetClamped` and `subtractOffsetClamped` preserve the bounded invariant by clamping at either 18-hour limit. `negateOffset` is exact because the bounds are symmetric. Offset arithmetic is separate from both fixed `Duration` arithmetic and calendar-relative `Period` application.

## Offset date-times

`OffsetDateTime calendar` associates a valid `CalendarDateTime calendar` with a valid `Offset`. The constructor remains private; `atOffset` is the public construction boundary. Because both components already carry their invariants, association cannot fail.

`toInstant` subtracts the offset from the local date-time to resolve one unique global instant. `fromInstant` performs the inverse operation for a requested calendar and offset, returning `Left (TargetCalendarOutOfRange ...)` only when the resulting local day falls outside that calendar's supported historical range.

`withOffset` preserves the instant and shifts the local date and time, including across midnight. `OffsetDateTime.withCalendar` preserves the instant, local time, and offset while changing only the calendar representation of the date; it retains the same target-range validation as other calendar conversion APIs.

## Date-time zones

`DateTimeZone` is an opaque in-memory zone model. `fixedDateTimeZone` creates a zone with one permanent offset. `dateTimeZone` creates a transition-based zone from an initial offset and a statically known list of `(nanosecondsSinceEpoch, Offset)` changes; an erased proof requires transition instants to be strictly increasing. `refineDateTimeZone` validates arbitrary `Instant` transition data at runtime and rejects duplicate or reversed transitions.

`zoneOffsetAt` selects the offset effective at an instant, with a transition's new offset taking effect exactly at its instant. `mapLocal` maps a `CalendarDateTime` explicitly to `Skipped`, `Unambiguous`, or `Ambiguous`. Ambiguous results are ordered by instant and retain every candidate, even for synthetic transition data that creates more than the usual two mappings.

This module deliberately models validated zone behavior without parsing an external time-zone database. TZDB loading remains a separate trust boundary that will construct `DateTimeZone` values through `refineDateTimeZone`.

## Gregorian calendar API

Import `IotaTime` to use the calendar-polymorphic API and its Gregorian implementation. `CalendarDate Gregorian` is the Gregorian date type; `Month` and `DayOfWeek` provide the named Gregorian components.

```idris
date : CalendarDate Gregorian
date = calendarDate 29 February 2000

components : (Year, Month, DayOfMonth)
components = yearMonthDay {calendar = Gregorian} date
```

### Date components

`Year`, `DayOfMonth`, and `WeekNumber` are opaque domain types rather than aliases for `Integer`:

- `DayOfMonth` accepts integer literals from 1 through 31. Literals such as `0` and `32` fail to compile. `refineDayOfMonth` validates an integer learned at runtime and returns `Either DayOfMonthError DayOfMonth`.
- `Year` is an opaque semantic integer. It is intentionally not restricted to positive values because calendars and intermediate arithmetic may use earlier astronomical years.
- `WeekNumber` is also an opaque semantic integer. Gregorian week-date arithmetic intentionally supports zero and negative week numbers for HodaTime compatibility.
- `yearValue`, `dayOfMonthValue`, and `weekNumberValue` explicitly expose integer representations when arithmetic is required. Component constructors remain hidden.

`day`, `month`, and `year` are typed civil accessors. `day` returns `DayOfMonth`; component assignment is not used to represent calendar arithmetic.

Calendar-relative arithmetic uses target-indexed `Period` values:

```idris
tomorrow : CalendarDate Gregorian
tomorrow = applyPeriod (days 1) (calendarDate 31 December 2000)

later : CalendarDate Gregorian
later = applyPeriod (months 2 <+> days 3) (calendarDate 31 January 2000)
```

`years`, `months`, `weeks`, and `days` accept signed integers, so subtraction needs no parallel API: `days (-2)` moves two days backward. `negatePeriod` reverses every component and `scalePeriod` multiplies every component.

Combining periods with `<+>` aggregates corresponding fields before application. Thus `months 1 <+> months 1` is a two-month period and moves January 31 directly to March 31 rather than clamping through February. Date fields apply from largest to smallest: years, months, weeks, then days. Every application returns a valid date; Gregorian results clamp at October 15, 1582.

The target index prevents applying unsupported units to a value. Date units require `HasCalendar target`, time units require `HasTime target`, and mixed periods require both capabilities. For example, `months 2 <+> minutes 20` can target a `CalendarDateTime`, but neither a date-only nor a time-only value. The hidden constructor prevents callers from bypassing those constraints. `ApplyPeriod.applyPeriod` is the sole public application operation.

## Local time and date-time

`Hour`, `Minute`, `Second`, and `Nanosecond` are opaque refined components. Their literal ranges are 0–23, 0–59, 0–59, and 0–999,999,999 respectively. Invalid literals passed to `localTime` fail compilation; `refineLocalTime` validates integers learned at runtime and returns `Either LocalTimeError LocalTime`.

```idris
late : LocalTime
late = localTime 23 30 0 0

wrapped : LocalTime
wrapped = applyPeriod (hours 2) late
-- 01:30:00

lateDateTime : CalendarDateTime Gregorian
lateDateTime = on late (calendarDate 31 January 2000)

advanced : CalendarDateTime Gregorian
advanced = applyPeriod (months 1 <+> hours 2) lateDateTime
-- March 1, 2000 at 01:30:00
```

Time-only periods wrap a `LocalTime` within its 24-hour day. On `CalendarDateTime`, date fields apply first from largest to smallest, then time fields apply and any positive or negative day carry adjusts the resulting date. `CalendarDate` supports only calendar units, `LocalTime` only time units, and `CalendarDateTime` both.

`calendarDate` requires an erased proof of `So (isValidGregorianDate day month year)`. Idris finds that proof automatically for valid literals. Invalid literals fail to compile:

```idris
invalidDate : CalendarDate Gregorian
invalidDate = calendarDate 29 February 2021
-- Compile error: Can't find an implementation for So False.
```

The public Gregorian operations are:

- `calendarDate`, `fromNthDay`, `fromWeekDate`, and `gregorianFromDays` construct dates under erased validity proofs. Statically invalid calls do not compile.
- `refineGregorianDate`, `refineGregorianNthDay`, `refineGregorianWeekDate`, and `refineGregorianDays` handle values first learned at runtime. They return `Either GregorianDateError (CalendarDate Gregorian)` and are the only fallible construction boundary.
- `isLeapYear`, `maxDaysInMonth`, and the `isValidGregorian...` predicates expose Gregorian rules and decision procedures.
- `dayOfWeek`, `next`, and `previous` provide calendar-polymorphic weekday navigation. Date-returning operations clamp at October 15, 1582, so they preserve the type's validity invariant without `Maybe`.
- `yearMonthDay`, `day`, `month`, and `year` expose typed civil components.
- `years`, `months`, `weeks`, `days`, and `applyPeriod` provide signed calendar-relative arithmetic.

`gregorianFromDays` and `toDays` convert relative to the March 1, 2000 epoch. Flat days before the public Gregorian boundary cannot be constructed without an impossible proof. The generic `fromDays` method carries the same calendar-specific proof requirement.

The negative compiler fixtures under `test/compile-fail/` verify that invalid component literals, forged component/date/period representations, invalid leap days, pre-changeover dates, absent fifth weekdays, pre-changeover flat days, and periods with unsupported target capabilities remain compile errors. Each fixture declares an expected diagnostic fragment so unrelated import or harness failures cannot produce false positives.

## Julian calendar API

`CalendarDate Julian` uses the proleptic every-fourth-year leap rule from the Julian calendar's introduction on January 1, astronomical year -44 (45 BC). Earlier dates and flat days before `-746631` are rejected. Its calendar-local flat day zero is March 1, 2000 Julian.

Julian components are nominally distinct from Gregorian components. Use `JulianMonth` and `JulianDayOfWeek` as their types, with constructors qualified through `JulianMonths` and `JulianWeekdays`:

```idris
leapDay : CalendarDate Julian
leapDay = julianDate 29 JulianMonths.February 1900

thirdMonday : CalendarDate Julian
thirdMonday = julianFromNthDay Third JulianWeekdays.Monday JulianMonths.January 2000
```

The public Julian operations mirror the proof-carrying Gregorian boundary:

- `julianDate`, `julianFromDays`, `julianFromNthDay`, and `julianFromWeekDate` require erased validity proofs, inferred automatically for valid literals.
- `refineJulianDate`, `refineJulianDays`, `refineJulianNthDay`, and `refineJulianWeekDate` validate values learned at runtime.
- `isJulianLeapYear`, `maxJulianDaysInMonth`, and the `isValidJulian...` predicates expose Julian rules.
- Calendar periods and mixed `CalendarDateTime Julian` periods use the same target-indexed period API as Gregorian values.

## Hebrew calendar API

`CalendarDate (Hebrew numbering)` supports both civil and scriptural month numbering through `HebrewCivil` and `HebrewScriptural`. Both representations identify the same dates and flat days; only `hebrewMonthNumber` changes its starting month. Hebrew flat days begin at 1 Tishri 1 (`-2103607` relative to the shared epoch).

Hebrew months are indexed by year. `HebrewMonths.AdarI` carries erased evidence that its year is a leap year, so selecting Adar I in a common year is unrepresentable:

```idris
leapAdar : CalendarDate HebrewCivil
leapAdar = hebrewDate 1 5784 HebrewMonths.AdarI

-- Does not compile: 5786 is a common year.
invalidAdar : CalendarDate HebrewCivil
invalidAdar = hebrewDate 1 5786 HebrewMonths.AdarI
```

The calendar implements the 19-year leap cycle, the Rosh Hashanah postponement rules, variable Cheshvan and Kislev lengths, and distinct civil/scriptural month numbers. Its public construction boundary follows the other calendars:

- `hebrewDate`, `hebrewFromDays`, `hebrewFromNthDay`, and `hebrewFromWeekDate` construct civil dates under erased proofs. Their primed variants select either numbering system.
- `refineHebrewDate`, `refineHebrewDays`, `refineHebrewNthDay`, and `refineHebrewWeekDate` validate runtime civil values; primed variants support either numbering.
- `refineHebrewMonth` converts a runtime `HebrewMonthName` into a month indexed by its year, rejecting Adar I in common years.
- `isHebrewLeapYear`, `daysInHebrewYear`, `maxHebrewDaysInMonth`, and the `isValidHebrew...` predicates expose Hebrew decision procedures.
- Month periods skip absent Adar I in common years. Year periods map leap-year Adar I into common-year Adar and clamp the day when necessary.
- Hebrew dates support weekday navigation, mixed `CalendarDateTime` periods, and the same capability constraints as Gregorian and Julian dates.

## Calendar conversion

`withCalendar` preserves the underlying absolute day and reinterprets it through the target calendar. The expected result type selects the target representation:

```idris
christmasJulian : Either CalendarConversionError (CalendarDate Julian)
christmasJulian =
	IotaTime.Calendar.withCalendar (calendarDate 25 December 2024)

newYearHebrew : Either CalendarConversionError (CalendarDate HebrewCivil)
newYearHebrew =
	IotaTime.Calendar.withCalendar (calendarDate 16 September 2023)
```

The result is an `Either` because each calendar has a different supported range; `TargetCalendarOutOfRange` reports the target name and absolute day. `IotaTime.CalendarDateTime.withCalendar` applies the same date conversion while preserving the `LocalTime` unchanged.
