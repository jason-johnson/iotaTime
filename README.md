# iotaTime

An Idris 2 time library based on Erik Naggum's "Long painful history of time".

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
