module CivilIslamicBeforeEpoch
-- EXPECT: Can't find an implementation for So (intToBool (prim__gte_Integer -503166 -503165)).

import IotaTime
import IotaTime.Calendar.Islamic

invalid : CalendarDate CivilIslamicBcl
invalid = IotaTime.Calendar.Islamic.civilFromDays (-503166)