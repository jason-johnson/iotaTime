-- EXPECT: IotaTime.TimeZone.MkTimeZone is private.

module ForgedTimeZone

import IotaTime

invalid : TimeZone
invalid = MkTimeZone