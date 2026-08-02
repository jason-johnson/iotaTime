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
- `src/IotaTime/Period.idr` — target-indexed calendar-relative periods
- `src/IotaTime/Calendar/Component.idr` — opaque and refined calendar component types
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
