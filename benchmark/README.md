# Gregorian conversion benchmark

This benchmark compares the current flat-day Gregorian representation with a
faithful HodaTime-shaped representation using one packed 16-bit entry for each
day in a 100-year Gregorian century. Its date value stores the 400-year cycle,
century number from 0 through 3, and day-in-century table index. The table is a
73,048-byte `Buffer`, populated once before validation and timing and never
mutated afterward.

The benchmark first separates projection from iotaTime's stored flat day,
construction plus projection of the cached representation, and projection from
an already cached representation. It then compares construction from epoch days
and Y/M/D, conversion to epoch days, small day shifts, weekday calculation,
comparison, and one-month shifts. Before timing, three complete eras are checked
against `gregorianCivilFromDays`. Paired workloads use matching result checksums
where both representations expose the same scalar result.

Run the default Chez backend from the repository root:

```sh
make benchmark
```

Run the native RefC backend:

```sh
make benchmark-refc
```

Table initialization uses `Data.Buffer`'s checked `IO` API outside the timed
region. Lookup uses the same backend `uint16` read primitive through a pure
function because the buffer is immutable after publication. Year, month, and
day are decoded with shifts and masks from the packed layout `yyyyyyymmmmddddd`.

Three uncontended runs on the Codespace Chez backend produced:

| Workload | Nanoseconds per operation |
| --- | ---: |
| Current stored flat day: project Y/M/D | 95-102 |
| Packed date: construct from days and project | 75-88 |
| Packed date: project stored table index | 65-72 |

A representative expanded Chez run produced:

| Operation | Flat day | Stored table index |
| --- | ---: | ---: |
| Construct from epoch days | 21 ns | 33 ns |
| Convert to epoch days | 23 ns | 20 ns |
| Construct from Y/M/D | 66 ns | 67 ns |
| Shift seven days | 16 ns | 32 ns |
| Calculate weekday | 23 ns | 29 ns |
| Compare dates | 19 ns | 49 ns |
| Shift one month | 144 ns | 121 ns |

One Codespace RefC run produced:

| Workload | Nanoseconds per operation |
| --- | ---: |
| Current stored flat day: project Y/M/D | 9,701 |
| Packed date: construct from days and project | 2,323 |
| Packed date: project stored table index | 1,416 |

On Chez, packed projection was 26-36% faster. On RefC, packed construction plus
projection was 4.18 times faster and projection from the stored table index was
6.85 times faster.

The stored-index representation is therefore not uniformly faster. It favors
component projection and civil month/year arithmetic. A flat day favors
comparison, weekday calculation, small day shifts, and construction from an
epoch day. Conversion back to epoch days is a multiply-and-add for the packed
representation, while it is already available in the flat representation; the
small Chez numbers are close enough to be sensitive to backend optimization.

On RefC, the distinction is larger because arbitrary-precision division is
expensive. In one expanded run, stored-index projection was 6.85 times faster
and construction from Y/M/D was 1.94 times faster. Construction from epoch days
was 8.11 times slower, conversion to epoch days 4.10 times slower, shifting
seven days 4.45 times slower, and weekday calculation 1.82 times slower.

The cached representation was verified over 438,291 consecutive days spanning
three complete Gregorian eras, and all timed checksums matched.

The packed table retains exactly 73,048 payload bytes (about 71.3 KiB), plus the
runtime's small buffer header and alignment overhead.