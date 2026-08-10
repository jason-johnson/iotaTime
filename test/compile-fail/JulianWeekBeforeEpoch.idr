module JulianWeekBeforeEpoch
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate Julian
invalid = IotaTime.Calendar.Julian.fromWeekDate
  (-1000000) Sunday 2000
