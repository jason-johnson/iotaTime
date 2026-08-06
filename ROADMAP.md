
# Roadmap

This roadmap compares iotaTime with HodaTime 1.1.0.0 in both directions. It
tracks APIs to port, improvements that should flow back to HodaTime, shared
opportunities neither library currently implements, and differences that are
intentional consequences of Idris 2 and Haskell.

In each directional section, a checked item means the destination library has
the capability. Shared opportunities remain unchecked until both libraries have
adopted them or the item is split into library-specific work.

## HodaTime to iotaTime

Features present in HodaTime that iotaTime should support.

- [x] Audit and fill standard/custom pattern and locale coverage. `parseWith`
  supplies HodaTime `parse'`-style caller defaults through typed parser state.
- [x] Add appropriate `Eq`, `Ord`, and `Show` implementations for compound
  values, including constructor-oriented displays that preserve opaque
  representations. `NFData` and `Hashable` remain intentionally absent because
  no Idris ecosystem use case currently justifies new dependencies or public
  hashing contracts.
- [x] Expose ISO week-date construction through the supported public surface.
- [x] Promote `OffsetDateTime` instant, offset, and calendar conversions.
- [x] Promote `ZonedDateTime` calendar conversion and local-time resolution
  policies.

## iotaTime to HodaTime

Features implemented in iotaTime that would strengthen HodaTime.

- [ ] Add exact `between` operations for dates, local times, and calendar
  date-times. The current iotaTime API returns canonical day/nanosecond periods
  that apply back to the end value without choosing a years/months policy.
- [ ] Add validated half-open intervals plus emptiness, overlap, adjacency,
  intersection, and connected union operations.
- [ ] Add injectable system and fixed clocks, with zoned-clock adapters, so
  application code does not need to call the process clock directly.
- [ ] Add an explicit time-zone provider contract for deterministic tests,
  embedded data, databases, and non-system zone sources.
- [ ] Add `OffsetDateTime.withOffset` and `ZonedDateTime.withZone` operations
  that preserve the represented instant.
- [ ] Add fixed-duration `ZonedDateTime` arithmetic that advances the timeline
  and then re-evaluates the active zone offset.
- [ ] Add direct zone-offset and zone-interval queries for an instant. iotaTime
  exposes half-open interval bounds, wall offset, optional exact savings, and
  abbreviation without exposing its zone representation.
- [ ] Add TZDB version metadata, canonical identifiers, aliases, and
  IANA/Windows mappings. iotaTime reads TZDB identity data from the platform
  provider and uses Windows ICU APIs for IANA/Windows identifier conversion.

## Shared opportunities

Features absent from both libraries that may belong in both after their
semantics and data sources are specified.

- [ ] Add a civil-epoch tabular Islamic variant while retaining leap-pattern
  types, and add Persian variants only where their supported ranges can be
  vouched for explicitly.
- [ ] Add an unbounded interval representation and define its relationship to
  the existing bounded half-open interval.
- [ ] Design configurable years/months period-difference decomposition. Exact
  day/nanosecond differences already avoid end-of-month ambiguity; calendar
  decomposition requires an explicit rounding and clamping policy.
- [ ] Define stable interchange before adding adapters: versioned textual or
  binary encodings for instants, offsets, calendar values, and zone IDs, then
  optional JSON and database mappings. These adapters must not expose internal
  representations or force serialization/database dependencies into the core
  packages.
- [ ] Add caching and richer discovery controls to time-zone providers where
  profiling or deployment requirements justify them.

## Intentional differences

These are corresponding capabilities rather than gaps to erase.

- HodaTime constructors such as `calendarDate` and `localTime` validate with
  `Maybe`. iotaTime rejects invalid static inputs with erased `So` proofs and
  validates runtime inputs with typed `Either` refiners.
- HodaTime uses `MonadThrow` exceptions for strict zoned resolution. iotaTime
  represents skipped, ambiguous, provider, and calendar-range failures as
  explicit data and `Either` values.
- HodaTime uses lenses for many observations and updates. iotaTime uses total
  projections and proof-preserving functions so hidden constructors cannot be
  bypassed.
- HodaTime relies on fixed-width machine integers where appropriate. iotaTime
  uses opaque semantic components and arbitrary-precision integers where that
  removes overflow and normalization states.
- HodaTime can derive many value instances opaquely. Public iotaTime equality
  and ordering used by external `So` proofs must remain definitionally
  reducible, so derivation is limited to instances that do not cross that
  boundary.
- HodaTime's `hour'`, `minute'`, and `second'` patterns update an intermediate
  Haskell `TimeInfo` through lenses. iotaTime's ordinary time fields already
  update its typed `TimeFields` accumulator, so separate primed adapters would
  duplicate the same operation.
- iotaTime keeps mutable fake clocks outside the supported core API;
  applications and test suites can implement them through `Clock`.

## Packaging and engineering

- [x] Decouple native-free use from mandatory Unix or Windows support builds
  through `iotaTime-pure` and `IotaTime.Pure`.
- [x] Keep constructors and raw representations outside the supported API and
  enforce that boundary with compile-fail tests.
- [x] Support explicit runtime providers while retaining convenient system
  time-zone and locale acquisition in the complete package.

## Not currently planned

`AnnualDate`, `YearMonth`, `OffsetDate`, `OffsetTime`, and `DateInterval` are
Noda Time concepts, not HodaTime 1.1.0.0 APIs. They should be added only for a
demonstrated iotaTime or HodaTime use case, not described as compatibility gaps.
