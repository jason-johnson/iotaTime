# Temporary Cleanup Notes

> Temporary working document. Delete this file after the listed cleanup work is complete.

## Correctness and semantic risks

### 1. Coptic reverse `DayNth` selectors - completed

`nthCopticDayOfMonth` can calculate a non-positive reverse occurrence in the
5/6-day epagomenal month. `dayOfMonthFromInteger` then clamps that value to day
1, after which `isValidCopticNthDay` may accept it.

Compute an unbounded `Integer` candidate, validate it against the actual month
length, and only then construct `DayOfMonth`. Add tests for absent `Last`,
`SecondToLast`, `ThirdToLast`, and `FourthToLast` occurrences in common and leap
epagomenal months.

Implemented by separating raw candidate calculation from `DayOfMonth`
construction, requiring validity evidence for `nthCopticDayOfMonth`, and adding
runtime and compile-fail coverage for reverse epagomenal occurrences.

### 2. Non-Gregorian locale month names - completed

`pMMMM'` and `pMMM'` currently overlay the locale's twelve Gregorian month
names onto every calendar by ordinal position. This can label Coptic Thout,
Persian Farvardin, or Islamic Muharram as January in the selected language.

Choose explicit semantics:

- introduce calendar-indexed localization data such as `CalendarLocale calendar`;
- restrict operating-system month-name localization to Gregorian and Julian; or
- use canonical calendar names where localized names are unavailable.

Do not describe ordinal Gregorian overlays as localized non-Gregorian names.

Implemented with an explicit `PatternMonthNameSource` policy. Gregorian and
Julian opt into operating-system locale month tables; all other calendars use
their canonical full and abbreviated names. Tests cover Coptic, Persian,
Islamic, and Hebrew behavior.

### 3. Under-constrained `CalendarPattern` instances - completed

`patternMonthLimit`, month-name lists, abbreviations, and numeric projections
are independent values. External instances can provide mismatched lengths or
out-of-range projections.

Consider a stronger interface based on:

```idris
patternMonthCount : Nat
patternMonthNames : Vect patternMonthCount String
patternMonthAbbreviations : Vect patternMonthCount String
patternMonthIndex : CalendarDate calendar -> Fin patternMonthCount
```

This is an API-breaking change and should be evaluated deliberately.

Implemented with a `Nat` month count, count-indexed `Vect` name tables, and a
`Fin` month projection. Gregorian locale-name selection carries erased evidence
that the selected calendar has twelve month-name slots. Custom `pMonthName`
tables retain an erased size equality for call-site inference while formatting
now indexes month vectors directly with `Fin`.

### 4. Invalid month normalization in `refinePatternDate` - completed

Calendar month conversion helpers commonly map every unmatched integer to the
last month. Pattern parsers enforce numeric bounds first, but the public
`refinePatternDate` interface method can be called directly with invalid month
numbers.

Return a typed failure for out-of-range values or accept a bounded month index.

Implemented with a shared month bounds refiner that returns `ValueOutOfRange`
before any calendar-specific integer conversion. Direct-interface tests cover
zero and above-limit values across every built-in calendar family, including
both Persian arithmetic rules and both Hebrew numbering schemes.

### 5. Deterministic custom name parsing - completed

Custom names may intentionally be many-to-one, such as alternating `"Odd"` and
`"Even"` month labels. Duplicate labels are therefore valid and parsing should
deterministically select the first matching table entry.

Exclude empty labels from parsing because they consume no input, and parse
prefix-overlapping labels longest-first. Keep the caller-provided `Vect` API
and formatting behavior unchanged; do not require unique names or a validated
wrapper.

Implemented in the shared named-choice parser with a stable longest-first sort
and exclusion of empty labels from parsing. Equal and duplicate labels retain
caller order, so intentionally many-to-one custom names select their first
matching entry. Tests cover repeated `"Odd"`/`"Even"` labels, overlapping
prefixes, and empty-label formatting without zero-width parsing.

### 6. Weekday text is consumed but not validated - completed

`pDayName` recognizes and consumes a weekday label but does not compare it with
the resulting date. A weekday inconsistent with the parsed date is accepted.

Either:

- add `parsedWeekday : Maybe (Fin 7)` to `DateFields` and validate it after date
  refinement; or
- document and name the operation clearly as a structurally consumed field.

Implemented as an opt-in strict alternative. `DateFields` carries an optional
`Fin 7` parsed weekday through a default implicit field, preserving existing
three-argument `MkDateFields` calls. `pVerifiedDayName`, `pddddVerified`,
`pdddVerified`, and their locale-aware variants reject a weekday inconsistent
with the refined date. Existing `pDayName`, `pdddd`, and `pddd` remain lenient.

### 7. `nameAt` hides invalid indexes - completed

`nameAt` returns an empty string for an exhausted table and maps non-positive
indexes to the first entry. This keeps formatting total while concealing
malformed `CalendarPattern` instances.

Prefer `Vect` indexed by `Fin` so invalid formatting states are unrepresentable.

Implemented by replacing `patternWeekdayNumber` with
`patternWeekdayIndex : CalendarDate calendar -> Fin 7`, changing weekday name
tables to `Vect 7 String`, and formatting through direct vector indexing.
`nameAt` has been removed, so external `CalendarPattern` instances cannot
provide an out-of-range weekday projection.

## Maintainability

### 8. Duplicate nth-weekday arithmetic

The same nine-way `DayNth` calculation appears in Gregorian, Julian, Coptic,
Islamic, Persian, arithmetic Persian, and Hebrew code.

Extract a shared helper that computes the raw candidate from `DayNth`, month
length, first offset, and last offset. Keep calendar-specific range validation
in each calendar module.

### 9. Duplicate `CalendarPattern` data and scaffolding

Islamic/civil Islamic and Persian/arithmetic Persian instances duplicate month
names, abbreviations, projections, and refinement structure.

Share constants and parameterized refinement helpers where this reduces drift
without obscuring calendar-specific behavior.

### 10. Broad fallback in `dateTimeConversion`

`dateTimeConversion` uses `Left _` from `dateConversion` to try a time field.
This is currently harmless because the only failure is `UnsupportedSpecifier`,
but it could silently discard future date-specific errors.

Use an explicit `UnsupportedSpecifier` match or make field lookup return
`Maybe`, constructing the final unsupported-specifier error once.

### 11. Combined test cases

Several `PatternCalendar` runtime cases combine unrelated calendars in one
Boolean expression. Split them into individual `RuntimeCase` values so failures
identify the affected calendar immediately.

## Suggested order

1. Fix Coptic reverse nth-weekday validation.
2. Define non-Gregorian locale-name semantics.
3. Strengthen `CalendarPattern` around `Nat`, `Vect`, and `Fin`.
4. Validate custom name tables.
5. Extract duplicated nth-weekday and calendar-pattern helpers.
6. Split broad tests and tighten fallback control flow.
