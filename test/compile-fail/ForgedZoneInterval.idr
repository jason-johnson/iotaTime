-- EXPECT: IotaTime.TimeZone.MkZoneInterval is private.

module ForgedZoneInterval

import IotaTime

invalid : ZoneInterval
invalid = MkZoneInterval