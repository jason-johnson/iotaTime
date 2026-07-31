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
```

## Gregorian calendar API

> **Transitional API:** The current Gregorian surface preserves HodaTime behavior while the interface is being ported and tested. Its `Maybe` constructors, raw public `CalendarDate`, and parallel `ValidatedDate` wrapper do not yet satisfy the project-wide design goal above. They are scaffolding for the next redesign, in which `CalendarDate` itself will be proof-carrying and invalid literal dates will fail to compile.

Import `IotaTime` to use the calendar-polymorphic API and its Gregorian implementation. `CalendarDate Gregorian` is the Gregorian date type; `Month` and `DayOfWeek` provide the named Gregorian components.

```idris
date : Maybe (CalendarDate Gregorian)
date = calendarDate 29 February 2000

components : Maybe (Year, Month, DayOfMonth)
components = map (yearMonthDay {calendar = Gregorian}) date
```

The public Gregorian operations are:

- `calendarDate day month year` validates a civil date and rejects dates before October 15, 1582.
- `fromNthDay` constructs the first through fifth, or last, requested weekday in a month.
- `fromWeekDate` uses Sunday-based arithmetic week numbering, matching HodaTime. Week one contains January 1; integer week numbers are not restricted to positive values.
- `isLeapYear` and `maxDaysInMonth` expose Gregorian calendar rules.
- `dayOfWeek`, `next`, and `previous` provide calendar-polymorphic weekday navigation.
- `yearMonthDay`, `month`, and the `day`, `monthl`, and `year` lenses expose date components. `monthl` is zero-based.

`fromDays` and `toDays` are raw, inverse representation conversions relative to the March 1, 2000 epoch. They intentionally represent dates before the public Gregorian boundary. Validity belongs to smart constructors; lens updates normalize overflowing components and clamp results at October 15, 1582. Consequently these operational lenses intentionally do not satisfy every traditional lens law: for example, setting day 40 on March 1, 2000 produces April 9, 2000.

### Validated dates

`ValidatedDate calendar` is an opaque wrapper for code that must carry calendar validity in the type. Existing `CalendarDate` operations remain available for HodaTime compatibility and raw representation work.

```idris
safeDate : Maybe (ValidatedDate Gregorian)
safeDate = validatedCalendarDate 29 February 2000

safeComponents : Maybe (Year, Month, DayOfMonth)
safeComponents = map validatedYearMonthDay safeDate
```

- `validateDate` checks an existing `CalendarDate`; `validatedFromDays` checks a raw day count.
- `validatedCalendarDate`, `validatedFromNthDay`, and `validatedFromWeekDate` are Gregorian smart constructors that return the opaque type directly.
- `validatedToDays`, `validatedYearMonthDay`, and `validatedDayOfWeek` inspect a validated date without discarding its guarantee.
- `updateValidated`, `nextValidated`, and `previousValidated` revalidate their results and return `Nothing` if an operation crosses the calendar's valid boundary.
- `forgetValidation` explicitly returns to the raw compatibility API. `validatedEquals` and `validatedCompare` delegate comparison to the underlying calendar date.

The optics in `IotaTime.Optics` use the dependency-free van Laarhoven representation. They can be consumed directly by code using the same rank-2 lens type.
