module CopticWeekBeforeEpoch
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate Coptic
invalid = IotaTime.Calendar.Coptic.fromWeekDate
  (-1000000) CopticWeekdays.Sunday 1716
