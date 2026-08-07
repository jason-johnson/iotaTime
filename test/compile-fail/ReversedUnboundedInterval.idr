module ReversedUnboundedInterval
-- EXPECT: Can't find an implementation for So False.

import IotaTime

invalid : UnboundedInterval
invalid = unboundedInterval (Just 1) (Just 0)