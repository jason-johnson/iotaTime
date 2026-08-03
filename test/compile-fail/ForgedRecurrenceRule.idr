-- EXPECT: IotaTime.DateTimeZone.MkRecurrenceRule is private.

module ForgedRecurrenceRule

import IotaTime.DateTimeZone

invalid : RecurrenceRule
invalid = MkRecurrenceRule (JulianWithLeap 500) 0 WallTime