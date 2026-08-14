module CivilIslamicBeforeEpoch
-- EXPECT: Can't find an implementation for So (

import IotaTime
import IotaTime.Calendar.Islamic

invalid : CalendarDate CivilIslamicBcl
invalid = IotaTime.Calendar.Islamic.civilFromDays (-503166)