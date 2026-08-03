-- EXPECT: IotaTime.DateTimeZone.MkZoneRecurrence is private.

module ForgedZoneRecurrence

import IotaTime.DateTimeZone

invalid : ZoneRecurrence
invalid = MkZoneRecurrence