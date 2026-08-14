module DisjointIntervalIntersection
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : Interval
invalid = intersection (interval 0 10) (interval 11 20)
