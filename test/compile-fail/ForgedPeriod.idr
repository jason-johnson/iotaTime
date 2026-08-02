module ForgedPeriod
-- EXPECT: MkPeriod is private.

import IotaTime

invalid : Period Instant
invalid = MkPeriod 0 0 0 0 0 0 0 0