-- EXPECT: Can't find an implementation for So

module UnorderedZoneTransitions

import IotaTime

invalid : DateTimeZone
invalid = dateTimeZone "Invalid" (transitionInfo zeroOffset False "STD")
  [(1, transitionInfo (offsetFromHours 1) True "DST"),
   (0, transitionInfo zeroOffset False "STD")]
