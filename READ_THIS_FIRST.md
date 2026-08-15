# Gregorian cache-table investigation

## Decision

iotaTime will retain the current flat `daysSinceEpoch : Integer`
representation for Gregorian dates. The HodaTime-style 100-year lookup table
and stored cycle/century/day-index representation are not planned.

The table makes civil component projection faster, especially with RefC, but
it does not make Gregorian dates uniformly faster. It moves costs into several
other common operations, increases every date's representation, and adds a
73,048-byte static table. The overall tradeoff is not compelling for Idris.

## Design tested

The benchmark implements the relevant HodaTime design rather than only placing
a lookup table in front of iotaTime's existing representation:

- One packed 16-bit entry for each day in a 100-year Gregorian century.
- 36,524 entries occupying 73,048 bytes (about 71.3 KiB).
- Packed layout `yyyyyyymmmmddddd`.
- A date represented by its 400-year cycle, century `0..3`, and day-in-century
	table index.
- Direct construction from Y/M/D and a fast path for shifts that remain within
	a century.
- Explicit handling of the extra leap day at the end of each 400-year cycle.

The table is allocated and populated once before timing. A pure backend
primitive reads its immutable 16-bit entries. The implementation was checked
against the existing Gregorian conversion over 438,291 consecutive days,
spanning three complete 400-year eras.

Unlike HodaTime, iotaTime supports arbitrary years from 1582 onward. HodaTime
can use compact fixed-width fields, while iotaTime would need at least an
arbitrary-precision `Integer` cycle plus century and index fields. The stored
representation would therefore be wider and more allocation-heavy than the
current single `Integer`.

## Results

A representative Chez run measured:

| Operation | Flat day | Stored table index | Result |
| --- | ---: | ---: | --- |
| Project Y/M/D | 110 ns | 64 ns | Table 42% faster |
| Construct from epoch days | 21 ns | 33 ns | Table 57% slower |
| Convert to epoch days | 23 ns | 20 ns | Approximately tied |
| Construct from Y/M/D | 66 ns | 67 ns | Approximately tied |
| Shift seven days | 16 ns | 32 ns | Table 2x slower |
| Calculate weekday | 23 ns | 29 ns | Table 26% slower |
| Compare dates | 19 ns | 49 ns | Table 2.6x slower |
| Shift one month | 144 ns | 121 ns | Table 16% faster |

Across several uncontended Chez projection runs, projection from a stored table
index took 65-72 ns instead of 95-102 ns. Deriving the table representation
from an epoch day and then projecting took 75-88 ns.

RefC makes arbitrary-precision division particularly expensive. One expanded
run found:

- Stored-index Y/M/D projection was 6.85x faster.
- Direct construction from Y/M/D was 1.94x faster.
- Construction from epoch days was 8.11x slower.
- Conversion to epoch days was 4.10x slower.
- Shifting seven days was 4.45x slower.
- Weekday calculation was 1.82x slower.

## Impact by workload

The stored-index design favors civil operations:

- Reading year, month, and day.
- Formatting and display.
- Month and year arithmetic.
- Calendar periods dominated by months and years.
- Recurring timezone rules that need Gregorian components.

The current flat-day design favors scalar and timeline operations:

- Equality, ordering, and sorting.
- Adding days or weeks.
- Weekday calculation and weekday navigation.
- Day-only differences.
- Construction from an epoch day.
- Instant and offset date-time conversion.
- Cross-calendar conversion.
- Persistence and scalar day formatting.

Public NodaTime, Java, and Rust code does not indicate one universal workload.
Infrastructure and service code is commonly timeline-heavy: persistence,
expiry, retries, queues, comparisons, and fixed-duration scheduling. Civil
projection is concentrated in formatting, reports, user input, recurring
wall-clock schedules, and date-oriented business applications. Source-search
counts are directional rather than runtime profiles, but they do not establish
that civil projection dominates enough workloads to justify slowing the other
operations globally.

## Conclusion

The table is a strong specialized optimization for repeated Gregorian component
projection, particularly on RefC. Switching the representation globally would,
however, exchange that gain for slower comparison, day arithmetic, bridge
conversion, and instant-facing operations. It would also increase date size and
introduce a permanent table and backend-specific immutable-buffer access.

For iotaTime's general-purpose Gregorian date, the simpler flat epoch-day
representation provides the better overall balance. More local improvements,
such as projecting Y/M/D once and reusing the tuple during formatting, can be
considered independently without changing the stored representation.

The executable benchmark and detailed methodology are in
[`benchmark/Main.idr`](benchmark/Main.idr) and
[`benchmark/README.md`](benchmark/README.md).
