-- EXPECT: IotaTime.TimeZone.Core.MkRecurrenceRule is private.

module ForgedRecurrenceRule

import IotaTime.TimeZone

invalid : RecurrenceRule
invalid = MkRecurrenceRule