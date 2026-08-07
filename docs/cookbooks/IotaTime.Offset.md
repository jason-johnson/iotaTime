## Using Offset

### Construct a known UTC offset

Statically known offsets are accepted only within the supported range of
minus 18 through plus 18 hours.

```idris
centralEuropeanOffset : Offset
centralEuropeanOffset = IotaTime.Offset.fromHours 1
```

### Refine an offset from input

Runtime values cross an explicit validation boundary. The result retains the
original invalid second count in `OffsetOutOfRange`.

```idris
runtimeOffset : Either OffsetError Offset
runtimeOffset = refineOffsetSeconds 19800
```

These declarations are compiled from `examples/GuideExamples.idr`.
