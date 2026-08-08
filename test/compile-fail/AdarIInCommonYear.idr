module AdarIInCommonYear
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate HebrewCivil
invalid = IotaTime.Calendar.Hebrew.calendarDate 1 5786 HebrewMonths.AdarI
