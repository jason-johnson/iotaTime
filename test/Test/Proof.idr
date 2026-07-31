module Test.Proof

import IotaTime

public export
zeroTickRoundTrip : ticks (MkInstant 0) = 0
zeroTickRoundTrip = Refl

public export
negativeTickRoundTrip : ticks (MkInstant (-1)) = -1
negativeTickRoundTrip = Refl

public export
largeTickRoundTrip : ticks (MkInstant 999999999999999999999999999999) = 999999999999999999999999999999
largeTickRoundTrip = Refl

public export
roundTripConstructor : MkInstant (ticks (MkInstant 42)) = MkInstant 42
roundTripConstructor = Refl
