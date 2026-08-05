-- EXPECT: IotaTime.OffsetDateTime.MkOffsetDateTime is private.

module ForgedOffsetDateTime

import IotaTime

invalid : OffsetDateTime Gregorian
invalid = MkOffsetDateTime
