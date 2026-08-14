-- EXPECT: IotaTime.TimeZone.Core.MkZoneRecurrence is private.

module ForgedZoneRecurrence

import IotaTime.TimeZone

invalid : ZoneRecurrence
invalid = MkZoneRecurrence