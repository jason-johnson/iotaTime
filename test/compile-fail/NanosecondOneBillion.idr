module NanosecondOneBillion
-- EXPECT: Can't find an implementation for So False.

import IotaTime

invalid : LocalTime
invalid = localTime 0 0 0 1000000000