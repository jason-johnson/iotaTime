-- EXPECT: IotaTime.ZonedDateTime.MkZonedDateTime is private.

module ForgedZonedDateTime

import IotaTime

invalid : ZonedDateTime Gregorian
invalid = MkZonedDateTime