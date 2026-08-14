module IslamicWeekBeforeEpoch
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate IslamicBcl
invalid = IotaTime.Calendar.Islamic.fromWeekDate
  (-1000000) Saturday 1443
