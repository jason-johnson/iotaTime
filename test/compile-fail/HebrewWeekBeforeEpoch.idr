module HebrewWeekBeforeEpoch
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate HebrewCivil
invalid = IotaTime.Calendar.Hebrew.fromWeekDate
  (-1000000) HebrewWeekdays.Sunday 5784
