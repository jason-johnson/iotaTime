-- EXPECT: IotaTime.TimeZone.Core.MkTimeZone is private.

module ForgedTimeZone

import IotaTime

invalid : TimeZone
invalid = MkTimeZone