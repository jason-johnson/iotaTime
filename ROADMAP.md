# Roadmap

This roadmap tracks substantive gaps between iotaTime and HodaTime. Idris-specific
differences such as erased validity proofs, typed refinement errors, explicit
time-zone resolution, and the Gregorian reform boundary are intentional design
choices rather than compatibility defects.

## High priority

- [x] Provide injectable system and fixed clocks for deterministic applications
  and tests.
- [ ] Add mutable fake-clock advancement and zoned-clock adapters.
- [ ] Add period-difference operations for dates, local times, and calendar date-times.
- [ ] Promote supported `OffsetDateTime` instant, offset, and calendar conversions.
- [ ] Promote supported `ZonedDateTime` zone conversion and fixed-duration arithmetic.
- [ ] Decouple pure library use from mandatory native Unix or Windows support builds.

## Medium priority

- [ ] Add `AnnualDate`, `YearMonth`, `OffsetDate`, `OffsetTime`, and `DateInterval`.
- [ ] Add interval overlap, intersection, union, adjacency, and unbounded endpoints.
- [ ] Expose TZDB version metadata, canonical identifiers, aliases, and platform mappings.
- [ ] Expose direct zone-offset and zone-interval queries.
- [ ] Add civil Islamic and additional vouched Persian calendar variants.
- [ ] Expand standard/custom patterns and locale data where typed use cases require it.
- [ ] Add stable serialization and database-oriented interchange adapters.
- [ ] Decide whether ISO week dates should join the supported public surface.

## Lower priority

- [ ] Add value instances for compound types where equality and ordering have clear
  domain semantics and remain compatible with proof reduction.
- [ ] Add caching and richer discovery controls to time-zone providers.
