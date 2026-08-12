-- EXPECT: IotaTime.TimeZone.Core.MkZoneInterval is private.

module ForgedZoneInterval

import IotaTime

invalid : ZoneInterval
invalid = MkZoneInterval