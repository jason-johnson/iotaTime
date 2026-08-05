-- EXPECT: IotaTime.DateTimeZone.MkDateTimeZone is private.

module ForgedDateTimeZone

import IotaTime

invalid : DateTimeZone
invalid = MkDateTimeZone