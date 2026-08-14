-- EXPECT: Undefined name MkTimeZoneCachePolicy.

module ForgedTimeZoneCachePolicy

import IotaTime

invalid : TimeZoneCachePolicy
invalid = MkTimeZoneCachePolicy
