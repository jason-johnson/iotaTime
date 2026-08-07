module ForgedUnboundedInterval
-- EXPECT: IotaTime.Interval.MkUnboundedInterval is private.

import IotaTime

invalid : UnboundedInterval
invalid = MkUnboundedInterval Nothing Nothing