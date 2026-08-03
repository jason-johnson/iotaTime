module Test.Proof

import IotaTime

data CalendarCapability = CalendarOnly
data TimeCapability = TimeOnly
data MixedCapability = CalendarAndTime

HasCalendar CalendarCapability where
	calendarCapability = ()
HasTime TimeCapability where
	timeCapability = ()
HasCalendar MixedCapability where
	calendarCapability = ()
HasTime MixedCapability where
	timeCapability = ()

calendarPeriod : Period CalendarCapability
calendarPeriod = months 2

timePeriod : Period TimeCapability
timePeriod = minutes 20

mixedPeriod : Period MixedCapability
mixedPeriod = months 2 <+> minutes 20

public export
zeroTickRoundTrip : ticks (fromNanosecondsSinceEpoch 0) = 0
zeroTickRoundTrip = instantNanosecondsRoundTrip 0

public export
negativeTickRoundTrip : ticks (fromNanosecondsSinceEpoch (-1)) = -1
negativeTickRoundTrip = instantNanosecondsRoundTrip (-1)

public export
largeTickRoundTrip :
	ticks (fromNanosecondsSinceEpoch 999999999999999999999999999999) =
		999999999999999999999999999999
largeTickRoundTrip =
	instantNanosecondsRoundTrip 999999999999999999999999999999

public export
roundTripConstructor :
	fromNanosecondsSinceEpoch (ticks (fromNanosecondsSinceEpoch 42)) =
		fromNanosecondsSinceEpoch 42
roundTripConstructor =
	cong fromNanosecondsSinceEpoch (instantNanosecondsRoundTrip 42)

public export
gregorianLeapCycle : isLeapYear 2000 = True
gregorianLeapCycle = Refl

public export
gregorianCenturyException : isLeapYear 2100 = False
gregorianCenturyException = Refl

public export
julianCenturyLeap : isJulianLeapYear 1900 = True
julianCenturyLeap = Refl

public export
hebrewLeapCycle : isHebrewLeapYear 5784 = True
hebrewLeapCycle = Refl
