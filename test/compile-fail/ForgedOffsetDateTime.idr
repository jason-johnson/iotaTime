-- EXPECT: IotaTime.OffsetDateTime.MkOffsetDateTime is private.

module ForgedOffsetDateTime

import IotaTime

invalid : OffsetDateTime Gregorian
invalid = MkOffsetDateTime
  (on (localTime 0 0 0 0) (calendarDate 1 March 2000))
  zeroOffset
