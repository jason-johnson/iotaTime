## Cookbook

### Represent an instant without range loss

`pInstantNanoseconds` uses an arbitrary-precision signed count relative to the iotaTime epoch. Append a unit literal when the surrounding protocol should state the unit explicitly.

```idris
instantNanosecondWire : Pattern Integer Instant
instantNanosecondWire = pInstantNanoseconds <% string "ns"
```

Offsets already have an exact standard pattern.

```idris
offsetWire : Pattern Offset Offset
offsetWire = pOffsetFull
```

### Agree on a calendar through the type

`pCalendarDays` represents an absolute day while the Pattern type fixes which calendar interprets it. Calendar details such as the civil Islamic epoch remain explicit without embedding a calendar tag in every value.

```idris
civilIslamicDayWire : Pattern Integer (CalendarDate CivilIslamicBcl)
civilIslamicDayWire = pCalendarDays {calendar = CivilIslamicBcl}
```

### Choose zone-ID syntax

Use the token form for whitespace-free IANA identifiers. Use the quoted form when identifiers may contain spaces or escaped characters, as Windows identifiers do.

```idris
ianaZoneWire : Pattern String String
ianaZoneWire = pZoneIdToken

windowsZoneWire : Pattern String String
windowsZoneWire = pZoneIdQuoted
```

These declarations are compiled from `examples/GuideExamples.idr`.