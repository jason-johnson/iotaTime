module ForgedLocalTime
-- EXPECT: MkLocalTime is private.

import IotaTime

invalid : LocalTime
invalid = MkLocalTime (-1)