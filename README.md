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

The optics preserve HodaTime's operational semantics. `day` focuses on a one-based `Integer` and `monthl` on a zero-based `Integer`, so transformations can cross component boundaries: modifying December 31's day by `(+ 1)` produces January 1, and setting March's day to 40 moves into April. `year` focuses on `Year`; because every integer year is representable, `Year` safely supports numeric arithmetic such as `(+ 1)`. Every setter normalizes to another valid `CalendarDate`.

`DayOfMonth` remains the refined civil component returned by `yearMonthDay` and accepted by `calendarDate`. The distinction is intentional: a standalone day-of-month must be in 1 through 31, while an intermediate operational lens value may overflow because the surrounding date supplies the context needed to normalize it.

Arithmetic rollover is explicit rather than encoded as an invalid component assignment:

- `normalizeDay ordinal` interprets an integer as a one-based day ordinal relative to the current month. For example, ordinal 40 in March normalizes into April.
- `shiftDays amount` moves by a number of flat days.
- `normalizeMonth ordinal` interprets an integer as a zero-based month ordinal relative to the current year, allowing values outside 0 through 11 to cross years.
- `shiftMonths amount` moves by a number of calendar months and clamps the day when necessary.

All four operations return a valid `CalendarDate`; Gregorian results clamp at October 15, 1582.

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
- `yearMonthDay` and `month` expose typed civil components; the operational `day`, `monthl`, and `year` lenses preserve HodaTime's normalizing update behavior.

`gregorianFromDays` and `toDays` convert relative to the March 1, 2000 epoch. Flat days before the public Gregorian boundary cannot be constructed without an impossible proof. The generic `fromDays` method carries the same calendar-specific proof requirement.

Lens updates normalize component combinations and clamp results at October 15, 1582. These operational lenses intentionally do not satisfy every traditional lens law: for example, setting January 31's month to February produces February 29 in a leap year. Arbitrary integer rollover uses the explicit normalization operations described above.

The negative compiler fixtures under `test/compile-fail/` verify that invalid component literals, forged component/date representations, invalid leap days, pre-changeover dates, absent fifth weekdays, and pre-changeover flat days remain compile errors. Each fixture declares an expected diagnostic fragment so unrelated import or harness failures cannot produce false positives.

The optics in `IotaTime.Optics` use the dependency-free van Laarhoven representation. They can be consumed directly by code using the same rank-2 lens type.
