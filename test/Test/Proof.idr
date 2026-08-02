module Test.Proof

import IotaTime

data CalendarCapability = CalendarOnly
data TimeCapability = TimeOnly
data MixedCapability = CalendarAndTime

HasCalendar CalendarCapability where
HasTime TimeCapability where
HasCalendar MixedCapability where
HasTime MixedCapability where

calendarPeriod : Period CalendarCapability
calendarPeriod = months 2

timePeriod : Period TimeCapability
timePeriod = minutes 20

mixedPeriod : Period MixedCapability
mixedPeriod = months 2 <+> minutes 20

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

public export
gregorianLeapCycle : isLeapYear 2000 = True
gregorianLeapCycle = Refl

public export
gregorianCenturyException : isLeapYear 2100 = False
gregorianCenturyException = Refl
