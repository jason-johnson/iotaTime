-- EXPECT: Undefined name .cacheNamedZones.

module ExposedTimeZoneCachePolicyField

import IotaTime

invalid : TimeZoneCachePolicy -> Bool
invalid policy = policy.cacheNamedZones
