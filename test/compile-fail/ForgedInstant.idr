module ForgedInstant
-- EXPECT: IotaTime.Instant.MkInstant is private.

import IotaTime

invalid : Instant
invalid = MkInstant 1
