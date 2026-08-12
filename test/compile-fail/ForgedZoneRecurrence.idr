-- EXPECT: IotaTime.TimeZone.MkZoneRecurrence is private.

module ForgedZoneRecurrence

import IotaTime.TimeZone

invalid : ZoneRecurrence
invalid = MkZoneRecurrence