module CivilIslamicWeekBeforeEpoch
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate CivilIslamicBcl
invalid = IotaTime.Calendar.Islamic.civilFromWeekDate
  (-1000000) IslamicWeekdays.Saturday 1443
