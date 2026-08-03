module OffsetHoursOutOfRange
-- EXPECT: Can't find an implementation for So (intToBool 1 && Delay (19 <= 18)).

import IotaTime

invalid : Offset
invalid = offsetFromHours 19
