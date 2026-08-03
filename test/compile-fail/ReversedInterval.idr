module ReversedInterval
-- EXPECT: Can't find an implementation for So (intToBool 0).

import IotaTime

invalid : Interval
invalid = interval 1 0
