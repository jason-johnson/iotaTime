-- EXPECT: Can't find an implementation for So

module UnorderedZoneTransitions

import IotaTime

invalid : DateTimeZone
invalid = dateTimeZone "Invalid" zeroOffset
  [(1, offsetFromHours 1), (0, zeroOffset)]
