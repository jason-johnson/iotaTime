# Roadmap

This roadmap tracks verified gaps between iotaTime and HodaTime's exposed API,
plus completed Idris-specific engineering work. It is audited against HodaTime
1.1.0.0. Differences such as erased validity proofs, typed refinement errors,
explicit time-zone resolution, and the Gregorian reform boundary are intentional
design choices rather than compatibility defects.

## High priority

- [x] Provide injectable system and fixed clocks for deterministic applications
  and tests.
- [x] Add zoned-clock adapters while keeping mutable fake clocks in test support.
- [x] Add exact period-difference operations for dates, local times, and calendar date-times.
- [x] Promote supported `OffsetDateTime` instant, offset, and calendar conversions.
- [x] Promote supported `ZonedDateTime` zone conversion and fixed-duration arithmetic.
- [x] Decouple pure library use from mandatory native Unix or Windows support builds.

## Medium priority

- [x] Add bounded interval overlap, intersection, connected union, and adjacency.
- [ ] Expand standard/custom patterns and locale data where typed use cases require it.
- [x] Expose ISO week-date construction through the supported public surface.

## Lower priority

- [ ] Add value instances for compound types where equality and ordering have clear
  domain semantics and remain compatible with proof reduction.
