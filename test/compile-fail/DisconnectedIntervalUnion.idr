module DisconnectedIntervalUnion
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : Interval
invalid = IotaTime.Interval.union (interval 0 10) (interval 11 20)
