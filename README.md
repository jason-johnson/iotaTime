# iotaTime

An Idris 2 time library based on Erik Naggum's "Long painful history of time".

## Design goal: make invalid time values unrepresentable

iotaTime is a port of HodaTime to Idris 2, but reproducing the Haskell API is only an intermediate step. The project exists to use dependent types to move calendar and time invariants out of runtime validation and into the type system.

## Supported API

The supported interface consists of declarations marked `public export` in
the modules listed by `docs/public-modules.json`. It follows HodaTime's exposed
modules, with additional proof predicates, erased-proof constructors, and
typed runtime refiners where Idris can express a stronger boundary.

Idris package files do not have Cabal's distinction between `exposed-modules`
and `other-modules`. Implementation modules therefore use non-public imports
and are omitted from generated API documentation. Some implementation records
must use `public export` inside those modules so sibling modules can access
their constructors and fields; importing such a module directly is unsupported.
Tests for a release exercise the supported modules rather than those internal
representations, parsers, or platform adapters.

### Version 0.2 migration

Version 0.2 narrows the supported API to HodaTime-compatible names plus
iotaTime's proof predicates, erased-proof constructors, typed runtime
refiners, and explicit provider boundaries. Earlier descriptive aliases and
the custom timezone/TZif/POSIX/Windows assembly APIs are no longer re-exported
from `IotaTime` or included in the generated public documentation. Use the
canonical names shown below. Applications should load zones with `utc`,
`timeZone`, or `localZone` rather than construct transition data directly.

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
- `ROADMAP.md` — prioritized HodaTime compatibility gaps
- `Makefile` — native support-library build and installation
- `support/iotatime_windows.c` — Win32 registry FFI and non-Windows stub
- `support/iotatime_unix.c` — POSIX locale FFI and Windows stub
- `src/IotaTime.idr` — library entry module
- `src/IotaTime/Instant.idr` — opaque points on the global nanosecond timeline
- `src/IotaTime/Clock.idr` — injectable system and fixed clocks
- `src/IotaTime/Duration.idr` — opaque fixed elapsed-time amounts
- `src/IotaTime/DateTimeZone.idr` — validated fixed and transition-based zones
- `src/IotaTime/Interval.idr` — proof-carrying half-open timeline intervals
- `src/IotaTime/Offset.idr` — bounded signed UTC offsets
- `src/IotaTime/OffsetDateTime.idr` — calendar-local date-times resolved by UTC offset
- `src/IotaTime/Period.idr` — target-indexed calendar-relative periods
- `src/IotaTime/ZonedDateTime.idr` — instant, zone, offset, and calendar kept consistent
- `src/IotaTime/Tzdb/Tzif.idr` — bounds-checked TZif v1-v4 decoder
- `src/IotaTime/Tzdb/Posix.idr` — validated POSIX future-rule parser
- `src/IotaTime/Tzdb/Provider.idr` — shared typed provider contract
- `src/IotaTime/Tzdb/Windows/Types.idr` — Windows registry models and errors
- `src/IotaTime/Tzdb/Windows.idr` — pure Windows TZI and Dynamic DST conversion
- `src/IotaTime/Tzdb/Windows/Platform.idr` — native Windows registry provider
- `src/IotaTime/Tzdb.idr` — TZif/Unix loading, public API, and platform dispatch
- `src/IotaTime/Time/Component.idr` — opaque, range-checked clock components
- `src/IotaTime/LocalTime.idr` — proof-carrying local time of day
- `src/IotaTime/CalendarDateTime.idr` — calendar date paired with local time
- `src/IotaTime/Calendar/Component.idr` — opaque and refined calendar component types
- `src/IotaTime/Calendar/Gregorian.idr` — proof-carrying Gregorian calendar
- `src/IotaTime/Calendar/Iso.idr` — ISO-8601 week-date construction
- `src/IotaTime/Calendar/Julian.idr` — proof-carrying Julian calendar
- `src/IotaTime/Calendar/Coptic.idr` — proof-carrying Coptic calendar
- `src/IotaTime/Calendar/Islamic.idr` — indexed tabular Islamic calendars
- `src/IotaTime/Calendar/Persian.idr` — bounded astronomical Persian calendar
- `src/IotaTime/Locale/Unix/Platform.idr` — native POSIX locale acquisition
- `src/IotaTime/Locale/Windows/Platform.idr` — native Win32 locale acquisition
- `src/IotaTime/Locale.idr` — opaque locale data, built-ins, and public acquisition
- `src/IotaTime/Pattern.idr` — composable typed parsing and formatting core
- `src/IotaTime/Pattern/Calendar.idr` — calendar-specific pattern refinement capability
- `src/IotaTime/Pattern/CalendarDate.idr` — calendar-polymorphic date patterns
- `src/IotaTime/Pattern/LocalTime.idr` — local-time field and standard patterns
- `src/IotaTime/Pattern/Duration.idr` — signed fixed-duration patterns
- `src/IotaTime/Pattern/Instant.idr` — ISO-8601 UTC instant patterns
- `src/IotaTime/Pattern/Offset.idr` — signed UTC-offset patterns
- `src/IotaTime/Pattern/CalendarDateTime.idr` — combined Gregorian date-time patterns
- `src/IotaTime/Pattern/OffsetDateTime.idr` — fixed-offset date-time patterns
- `src/IotaTime/Pattern/ZonedDateTime.idr` — format-only zoned patterns and effectful parsing
- `src/IotaTime/Pattern/Locale.idr` — locale `strftime` layout compilation
- `examples/ZonedMeeting.idr` — installed-package zoned parsing example
- `RELEASE.md` — release validation and versioning checklist
- `test/iotaTime-test.ipkg` — test package
- `test/Main.idr` — test orchestrator
- `test/Test/Proof.idr` — compile-time proof tests
- `test/Test/Runtime.idr` — runtime behavior tests
- `test/Test/Regression.idr` — regression tests
- `test/Test/Support.idr` — runtime assertions and suite reporting
- `.github/workflows/ci.yml` — Linux full-suite and Windows registry CI matrix

## Build and test

Building the package requires `make` and a C compiler. On Windows, use MinGW GCC so the support library can link against `advapi32`; the CI workflow installs this toolchain through MSYS2.

```bash
idris2 --build iotaTime.ipkg
idris2 --install iotaTime.ipkg
idris2 --build test/iotaTime-test.ipkg
./test/build/exec/iotaTime-test
sh test/run-compile-fail-tests.sh
idris2 --build examples/iotaTime-examples.ipkg
./examples/build/exec/iotaTime-zoned-meeting
```

The example loads `Europe/Zurich` through the system provider, parses `2024-04-23T09:00:00 Europe/Zurich` with strict local-time resolution, and formats the resolved value back through `pZonedDateTime`.

## API documentation

Generate the declaration-level HTML reference, grouped API pages, and guide:

```bash
npm ci --prefix docs
idris2 --mkdoc iotaTime.ipkg
node docs/enhance-docs.mjs build/docs
```

Open `build/docs/index.html` after generation. `build/docs/guide.html` contains
the conceptual guide and compiled iotaTime equivalents of HodaTime's examples;
the module pages retain Idris-generated types and signatures while grouping
related declarations under curated headings.

Owner-authored pull requests publish temporary API-documentation previews via
GitHub Pages. Version tags publish source and documentation archives as a
GitHub Release and submit the release commit to the Idris 2 `pack` collection;
see `RELEASE.md` for the one-time repository setup.

Zoned parsing accepts provider callbacks in any `Monad`, so applications may
compose it with `IO`, `Control.App`, or an external effects interpreter without
iotaTime depending on a particular effects package. Operations that directly
read the system clock, locale database, filesystem, or Windows registry remain
explicit `IO` boundaries.

## Instants and fixed durations

`Instant` is an opaque point on the global timeline, stored canonically as an arbitrary-precision nanosecond count relative to March 1, 2000 UTC. `Duration` is a distinct opaque fixed amount measured in nanoseconds. Neither representation can overflow or requires carry normalization.

```idris
start : Instant
start = fromSecondsSinceUnixEpoch 0

finish : Instant
finish = IotaTime.Instant.add start (IotaTime.Duration.fromMinutes 90)

elapsed : Duration
elapsed = difference finish start
```

The HodaTime names `fromNanoseconds` through `fromStandardWeeks`, plus `add` and `minus`, are canonical within `IotaTime.Duration`; `IotaTime.Instant.add` and `IotaTime.Instant.minus` perform fixed timeline arithmetic. Module qualification disambiguates names such as `Duration.fromHours` and `Offset.fromHours` when using the umbrella import.

`fromStandardDays` and `fromStandardWeeks` mean exact 24-hour and seven-day amounts. Calendar-relative `days`, `weeks`, `months`, and `years` continue to construct target-indexed `Period` values instead. `now` reads the UTC system clock directly, while applications can accept any `Clock` implementation for deterministic behavior:

```idris
timestamp : Clock clock => clock -> IO Instant
timestamp = getCurrentInstant

deterministicClock : FixedClock
deterministicClock = fixedClock epoch
```

`systemClock` provides the production implementation. Unix conversion is available at whole-second and nanosecond precision.

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
india = IotaTime.Offset.fromMinutes 330

minimumOffset : Offset
minimumOffset = IotaTime.Offset.fromHours (-18)
```

The HodaTime names `empty`, `fromSeconds`, `fromMinutes`, `fromHours`, `seconds`, `minutes`, `hours`, `addClamped`, and `minusClamped` are available from `IotaTime.Offset`. Proof-carrying constructors reject statically known out-of-range values rather than reproducing HodaTime's constructor clamping.

`refineOffsetSeconds` validates an arbitrary runtime total and returns `Either OffsetError Offset`. The component accessors are sign-consistent, so `-01:30:45` yields `(-1, -30, -45)`.

`addClamped` and `minusClamped` preserve the bounded invariant by clamping at either 18-hour limit. Offset arithmetic is separate from both fixed `Duration` arithmetic and calendar-relative `Period` application.

## Offset date-times

`OffsetDateTime calendar` associates a valid `CalendarDateTime calendar` with a valid `Offset`. The constructor remains private. HodaTime-compatible construction uses `fromCalendarDateTimeWithOffset` or `fromInstantWithOffset`; the latter returns `Either` when the requested calendar cannot represent the local result.

`toCalendarDateTime` and `offset` expose the HodaTime-compatible observers.

## Date-time zones

`TimeZone` is the HodaTime-compatible name for the opaque in-memory zone model; `DateTimeZone` remains an alias. Applications acquire zones through the platform provider rather than constructing transitions directly. The implementation uses bounds-checked TZif decoding, validated POSIX future rules, and native Windows registry data, but those parsers and assembly types are internal modules rather than supported consumer API.

On Unix-like systems, the HodaTime-compatible names have typed effect signatures:

```idris
utc : IO (Either TzdbError TimeZone)
timeZone : String -> IO (Either TzdbError TimeZone)
localZone : IO (Either TzdbError TimeZone)
availableZones : IO (Either TzdbError (List String))
```

`TZDIR` overrides `/usr/share/zoneinfo`; `TZ` overrides `/etc/localtime` for the local zone. Named zones cannot escape the TZDB root. `availableZones` reports files that successfully decode as TZif rather than relying on filename conventions.

`TimeZoneProvider` isolates platform discovery. The `utcWith`, `timeZoneWith`, `localZoneWith`, and `availableZonesWith` variants accept an explicit provider; the canonical names use `systemTimeZoneProvider`. Unix filesystem discovery is built in. On Windows, internal registry models decode `REG_TZI_FORMAT`, `SYSTEMTIME`, and Dynamic DST history with typed malformed-data and unknown-zone failures.

On Windows, `systemTimeZoneProvider` uses `windowsNativeRegistrySource`. A small C support library calls `RegOpenKeyExW`, `RegEnumKeyExW`, and `RegQueryValueExW` from the native Win32 API. It reads the local zone, installed zone definitions, and Dynamic DST history without launching another process. Idris owns parsing, validation, recurrence construction, and all timezone calculations; the C boundary only acquires registry values. Native buffers are copied immediately and freed explicitly.

`iotaTime.ipkg` builds and installs `libiotatime_windows` through package hooks. Windows receives the Win32 implementation; other systems receive a small explicit unsupported-platform stub so the same package remains buildable everywhere. Idris copies the support library into downstream Chez executables through its normal C FFI packaging. This is platform dispatch, not source-level conditional compilation: `System.Info.isWindows` selects the Windows or Unix provider, and only the selected provider performs platform I/O.

CI runs the complete runtime and compile-fail suites on Linux. A native `windows-latest` matrix entry bootstraps Idris 2 using its upstream MSYS2/Chez recipe and runs `test/windows-smoke.ipkg` against the live Windows registry. The smoke suite verifies UTC, registry enumeration, local-zone resolution, missing-zone errors, and the historical 2007 US Dynamic DST change.

## Zoned date-times

`ZonedDateTime calendar` stores a zone together with the local date-time and effective offset for one instant. Its constructor is private. `fromInstant` uses HodaTime's instant-first argument order. `fromCalendarDateTimeAll`, `fromCalendarDateTimeStrictly`, and `fromCalendarDateTimeLeniently` retain the recognizable HodaTime construction policies, with typed `Either` errors replacing exceptions or hidden partiality. `resolveLocal` is the fully explicit Idris mapping API.

`toCalendarDateTime`, `toCalendarDate`, `toLocalTime`, `toInstant`, `inDst`, `zoneAbbreviation`, `zoneId`, and the direct date/time component functions match HodaTime's accessor vocabulary. `ZonedDateTime.withCalendar` preserves the instant and zone while changing the calendar representation, returning `Either` when the target calendar cannot represent the resulting local day.

Calendar-relative period arithmetic is intentionally absent: callers convert with `toCalendarDateTime`, apply the `Period`, then explicitly choose a local mapping policy. `ZonedDateTime` therefore does not implement `ApplyPeriod`.

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

`on time date` constructs a calendar date-time with time-first argument order. The HodaTime-compatible `at date time` provides date-first order, while `atStartOfDay date` uses midnight.

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

## ISO week-date API

ISO week dates reuse `CalendarDate Gregorian`; they differ only in week numbering. ISO weeks start on Monday, and week 1 is the week containing January 4. Import `IotaTime.Calendar.Iso` explicitly and qualify `fromWeekDate` to distinguish it from the Gregorian Sunday-based constructor:

```idris
isoNewYear : CalendarDate Gregorian
isoNewYear = IotaTime.Calendar.Iso.fromWeekDate 1 Monday 2020
-- December 30, 2019
```

`fromWeekDate` requires erased evidence of `isValidIsoWeekDate`. `refineIsoWeekDate` validates runtime values and returns `Either IsoWeekDateError (CalendarDate Gregorian)`. Arithmetic week zero and negative week numbers remain supported when their resulting dates are within the Gregorian range.

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

## Coptic calendar API

`CalendarDate Coptic` supports the 13-month Coptic calendar from 1 Thout 1. Years whose number is congruent to 3 modulo 4 are leap years. The first 12 months have 30 days; `CopticMonths.PiKogiEnavot` has five days in common years and six in leap years.

```idris
nayrouz : CalendarDate Coptic
nayrouz = copticDate 1 CopticMonths.Thout 1738

leapDay : CalendarDate Coptic
leapDay = copticDate 6 CopticMonths.PiKogiEnavot 1731
```

`copticDate`, `copticFromDays`, `copticFromNthDay`, and `copticFromWeekDate` reject invalid static values through erased proofs. Their `refineCoptic...` counterparts return `Either CopticDateError` for runtime input. Date periods clamp at the year-1 boundary and at the shorter epagomenal month.

## Islamic calendar API

`CalendarDate (Islamic pattern)` implements the tabular Islamic calendar with the astronomical epoch, 1 Muharram 1 = July 18, 622 proleptic Gregorian. Odd months have 30 days, even months have 29, and Dhul Hijjah gains day 30 in a leap year.

The leap pattern is part of the calendar type. `IslamicBcl` and `IslamicBase16` use the Base16 pattern compatible with .NET and NodaTime; `IslamicBase15`, `IslamicIndian`, and `IslamicHabashAlHasib` select the other common 30-year patterns. Values from different patterns have different types and cannot be mixed.

```idris
defaultLeapDay : CalendarDate IslamicBcl
defaultLeapDay = islamicDate 30 IslamicMonths.DhulHijjah 16

base15LeapDay : CalendarDate IslamicBase15
base15LeapDay = islamicDate' {pattern = Base15}
	30 IslamicMonths.DhulHijjah 15
```

The unprimed constructors and refinements use `IslamicBcl`. Primed forms such as `islamicDate'`, `islamicFromNthDay'`, and `refineIslamicDate'` select a pattern through the expected type or an explicit `{pattern = ...}` argument. Static invalid dates require impossible erased proofs; runtime inputs return `Either IslamicDateError`.

## Persian calendar API

`CalendarDate Persian` implements the official astronomical Solar Hijri calendar over Persian years 1 through 1500. The first six months have 31 days, the next five have 30, and Esfand has 29 or 30 according to the astronomical leap assignment.

```idris
nowruz : CalendarDate Persian
nowruz = persianDate 1 PersianMonths.Farvardin 1404

leapDay : CalendarDate Persian
leapDay = persianDate 30 PersianMonths.Esfand 1403
```

The leap-year table is generated from HodaTime's Meeus equinox, equation-of-time, and Espenak-Meeus delta-T calculation for its vouched range. Embedding those results makes behavior deterministic across backends and keeps static proofs reducible without running floating-point astronomy during compilation or at runtime. In particular, astronomical Nowruz 1404 is March 21, 2025, unlike the arithmetic calendar's March 20 result.

`persianDate`, `persianFromDays`, `persianFromNthDay`, and `persianFromWeekDate` require erased validity proofs. Their `refinePersian...` counterparts return `Either PersianDateError` for runtime values. Period arithmetic clamps at both supported-year boundaries.

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

## Calendar date patterns

`Pattern state value` combines formatting with full-input parsing. Fields compose with `<+>`, and `<%` appends a literal produced by `char` or `string`. The parsing engine uses `Data.String.Parser` from Idris 2's `contrib` package, while the public boundary returns `Either PatternError value`; malformed fields, values outside their field ranges, invalid final dates, and trailing input remain distinct typed failures. Formatting is specialized to `value -> String`, which provides the pattern-specific composition supplied by Haskell's `Formatting` and `HoleyMonoid` machinery without introducing a general variadic formatting layer.

`CalendarPattern calendar` supplies calendar-specific numeric projection, canonical month names, month limits, weekday numbering, and runtime date refinement. Instances cover Gregorian, Julian, Coptic, Persian, every indexed Islamic leap pattern, and both Hebrew numbering systems. Parsed year, month, and day fields therefore cross each calendar's existing typed refinement boundary rather than constructing a generic unchecked date.

The calendar-date layer provides the HodaTime-compatible numeric fields `pyear`, `pyyyy`, `pyy`, `pmonthNum`, `pMM`, `pday`, `pdd`, and `pdaySpace`. Canonical calendar names are available through `pMMM` and `pMMMM`; `pddd` and `pdddd` use weekday names. Parsing is case-insensitive, and weekday fields are consumed without redundantly validating the date. `pd` is the slash-separated short date, `pD` is the canonical long date, `pR` is the numeric round-trip pattern, and `pmonthDay` and `pyearMonth` provide partial layouts:

```idris
isoText : String
isoText = format (pR {calendar = Gregorian}) (calendarDate 3 March 2020)

parsed : Either PatternError (CalendarDate Gregorian)
parsed = parse (pR {calendar = Gregorian}) "2020-03-03"

coptic : Either PatternError (CalendarDate Coptic)
coptic = parse (pR {calendar = Coptic}) "1731-13-06"

hebrewNamed : Either PatternError (CalendarDate HebrewCivil)
hebrewNamed = parse
	(((pyyyy {calendar = HebrewCivil} <% char '-') <+>
		(pMMMM {calendar = HebrewCivil} <% char '-')) <+>
		pdd {calendar = HebrewCivil})
	"5784-AdarI-01"

unpadded : Pattern DateFields (CalendarDate Gregorian)
unpadded =
	((pyear {calendar = Gregorian} 1 <% char '-') <+>
		(pmonthNum {calendar = Gregorian} 1 <% char '-')) <+>
	pday {calendar = Gregorian} 1

longText : String
longText = format (pD {calendar = Gregorian}) (calendarDate 3 March 2020)
```

Each independently composed generic field may need `{calendar = ...}` because Idris elaborates operands before `<+>` combines them. Standard whole patterns usually need the calendar annotation only once.

`pMonthName` and `pDayName` remain Gregorian custom-table APIs accepting `Vect 12 String` and `Vect 7 String`. Unlike Haskell's list-based API, incomplete tables therefore fail to compile rather than failing during formatting. Locale-aware primed fields are also Gregorian because operating-system locale snapshots contain exactly 12 Gregorian month names.

Field parsers accumulate raw components in `DateFields` and call `refinePatternDate` only after consuming the complete input. Custom field order is therefore independent of temporary invalid dates, while Gregorian `"2021-02-29"`, Coptic `"1730-13-06"`, or common-year Hebrew Adar I still fail at the runtime trust boundary.

## Local time patterns

`phour`, `pminute`, and `psecond` provide configurable-width 24-hour fields; `pHH`, `pmm`, and `pss` are their two-digit forms. `pt` is `HH:mm`, `pT` is `HH:mm:ss`, and `pr` adds all nine nanosecond digits.

`phh` and `phhSpace` use the 12-hour clock. `pp`, `ppp`, `pPeriod`, and locale-aware `ppp'` provide AM/PM designators. Parsing is field-order independent: both `03:04 PM` and `PM 03:04` resolve to 15:04 when composed in the corresponding order.

`pfrac width` formats and parses fixed-width fractional seconds, scaling parsed digits to nanoseconds. Its erased `So (isValidFractionWidth width)` argument restricts widths to 1 through 9, so `pfrac 0` and `pfrac 10` fail at compile time.

## Duration patterns

`pDuration` formats and parses fixed durations as `[-]D:HH:mm:ss`; `pDurationNano` adds exactly nine fractional digits as `[-]D:HH:mm:ss.fffffffff`. Day counts use arbitrary-precision integers, matching the underlying `Duration` representation.

The optional sign belongs to the complete duration rather than an individual field. Parsing therefore reads the layout as one quantity and constructs the result through `fromNanoseconds`, preserving canonical negative values such as `-0:00:00:00.000000001` without exposing the `Duration` representation.

## Instant patterns

`pInstant` parses and formats UTC instants as `yyyy-MM-ddTHH:mm:ssZ`; `pInstantNano` includes exactly nine fractional digits. `instantPattern` adapts any Gregorian `CalendarDateTime` pattern and treats its value as UTC.

Use `parseInstant` for the typed `Either PatternError Instant` parsing boundary. Because `Instant` has an arbitrary-precision timeline while the proof-carrying Gregorian calendar begins on October 15, 1582, `formatInstant` returns `Either CalendarConversionError String`. An earlier instant therefore reports `TargetCalendarOutOfRange` rather than hiding a partial conversion inside the total `Pattern.format` API.

## Zoned date-time patterns

`pZonedDateTime` formats a zoned value in its calendar as a numeric local date-time followed by its zone ID, such as `2024-04-23T09:00:00 Europe/Zurich` or `1731-13-06T01:02:03 UTC`. `zonedDateTimePattern` adapts another calendar-polymorphic `CalendarDateTime` pattern and a zone-suffix renderer. Both produce the dedicated format-only `ZonedDateTimePattern`, so the pure `Pattern.parse` API cannot accidentally construct a zoned value without loading its rules or choosing a local-time resolution policy.

`parseStandardZonedDateTime` parses the standard layout. The caller supplies an effectful zone provider and an explicit resolver such as `fromCalendarDateTimeStrictly` or `fromCalendarDateTimeLeniently`; `parseZonedDateTimeWith` provides the same mechanism for a custom local pattern. `ZonedDateTimePatternError` keeps structural parsing, provider lookup, and skipped or ambiguous time resolution failures distinct. The standard parser has a distinct name in the umbrella API because locale `%Z` parsing already uses `parseZonedDateTime`.

## Offset and date-time patterns

`pOffset` formats signed hours and minutes as `+02:00`; `pOffsetFull` includes seconds, `pOffsetZ` writes `Z` for UTC, and `pOffsetCompact` implements the `strftime` `%z` form such as `+0200`. Parsing reads the sign and all components as one quantity, then calls `refineOffsetSeconds`, so malformed components and values outside the supported plus-or-minus 18-hour range remain typed runtime failures.

`pairPattern` combines two independently refined patterns through projections and a constructor. `calendarDateTimePattern` uses it for any `CalendarPattern` date and local time; the standard `ps`, `po`, `pf`, `pF`, `pg`, and `pG` layouts mirror HodaTime. `offsetDateTimePattern` then combines any such local pattern with an offset pattern. `pOffsetDateTime` is the calendar-polymorphic numeric layout `yyyy-MM-ddTHH:mm:ss(+/-)HH:mm`.

## Locale API

`Locale` stores Gregorian month and weekday names, AM/PM designators, and the operating system layout strings needed by later whole-layout pattern compilation. Its constructor is private. Month tables use `Vect 12 String` and weekday tables use `Vect 7 String`, so every locale is structurally complete.

`enUS`, `deDE`, and `jaJP` provide pure built-ins. Read-only accessors include `localeId`, `monthNames`, `monthNamesShort`, `dayNames`, `dayNamesShort`, `amName`, and `pmName`. The locale-aware date fields `pMMMM'`, `pMMM'`, `pdddd'`, and `pddd'` select the corresponding names:

```idris
germanMonth : String
germanMonth = format (pMMMM' deDE) (calendarDate 3 March 2020)

japaneseMonth : String
japaneseMonth = format (pMMMM' jaJP) (calendarDate 3 March 2020)
```

Named locale fields parse case-insensitively. As with the fixed English weekday fields, locale weekday names are consumed but not validated against the resolved date.

On Unix, `localeByName` reads an installed locale through `newlocale` and `nl_langinfo_l`, while `currentLocale` follows `LC_ALL`, `LC_TIME`, and `LANG` and falls back to the POSIX `C` locale. The per-locale C APIs do not mutate process-global locale state.

On Windows, both functions read through `GetLocaleInfoEx`. Windows date and time picture strings are translated into the supported `strftime` subset, and Monday-first Win32 weekday tables are normalized to the library's Sunday-first order. `C` and `POSIX` names select the Windows invariant locale.

Both platforms return `IO (Either LocaleError Locale)`, keeping unknown names and platform failures explicit at the native trust boundary. Native snapshots are copied before their handles are freed.

`localeDatePattern` compiles a locale's date layout into a bidirectional Gregorian pattern. `compileDatePattern` accepts an explicit `strftime` layout. Date conversion support includes `%Y`, `%y`, `%m`, `%d`, `%e`, `%B`, `%b`, `%h`, `%A`, and `%a`, plus `%%`, `%n`, `%t`, and the composite `%F` and `%D` layouts. Unsupported conversions return `Left (UnsupportedSpecifier value)` and a trailing bare percent returns `Left DanglingPercent`.

`localeTimePattern` similarly compiles a locale's time layout into a bidirectional `LocalTime` pattern, while `compileTimePattern` accepts an explicit layout. Time conversions include `%H`, `%I`, `%l`, `%M`, `%S`, and `%p`; the tokenizer also expands the composite `%T`, `%R`, and `%r` layouts.

`localeDateTimePattern` compiles the combined locale layout into a Gregorian `CalendarDateTime` pattern, and `compileDateTimePattern` accepts an explicit combined layout. Date and time fields share a `DateTimeFields` accumulator, so their order is independent. Because `CalendarDateTime` represents civil time without a zone, `%Z` and `%z` fields and their preceding layout spaces are deliberately omitted.

`compileOffsetDateTimePattern` compiles a combined layout containing `%z` into a pure bidirectional `OffsetDateTime` pattern, and `localeOffsetDateTimePattern` applies it to a locale's combined layout. A missing `%z` returns `MissingOffsetSpecifier`.

`parseZonedDateTime` handles locale layouts containing `%Z`. It parses the local fields and one non-whitespace zone token, asks a caller-supplied provider to load that abbreviation, then applies a caller-supplied resolver such as `fromCalendarDateTimeStrictly` or `fromCalendarDateTimeLeniently`. `ZonedPatternError` keeps layout, structural parse, provider, and resolver failures distinct and preserves the caller's error types. A layout without `%Z` returns `MissingZoneSpecifier`.

Machine locale acquisition and locale-driven date, time, and combined date-time patterns are available on Unix and Windows.

Operating-system locale date layouts remain Gregorian because `Locale` intentionally stores complete `Vect 12` Gregorian month tables. Canonical and numeric patterns outside the locale compiler are calendar-polymorphic, including date, local date-time, offset date-time, and zoned date-time patterns.

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
