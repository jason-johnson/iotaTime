module ForgedInterval
-- EXPECT: IotaTime.Interval.MkInterval is private.

import IotaTime

invalid : Interval
invalid = MkInterval epoch epoch
