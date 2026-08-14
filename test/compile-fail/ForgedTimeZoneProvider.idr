-- EXPECT: Undefined name MkTimeZoneProvider.

module ForgedTimeZoneProvider

import IotaTime

invalid : TimeZoneProvider
invalid = MkTimeZoneProvider
