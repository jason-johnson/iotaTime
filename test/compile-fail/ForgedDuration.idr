module ForgedDuration
-- EXPECT: IotaTime.Duration.MkDuration is private.

import IotaTime

invalid : Duration
invalid = MkDuration 1
