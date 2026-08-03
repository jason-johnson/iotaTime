-- EXPECT: IotaTime.ZonedDateTime.MkZonedDateTime is private.

module ForgedZonedDateTime

import IotaTime

invalid : ZonedDateTime Gregorian
invalid = MkZonedDateTime
  (atOffset
    (on (localTime 0 0 0 0) (calendarDate 1 March 2000))
    (offsetFromHours 18))
  (fixedDateTimeZone "UTC" zeroOffset)
