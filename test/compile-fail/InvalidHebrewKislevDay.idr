module InvalidHebrewKislevDay
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate HebrewCivil
invalid = IotaTime.Calendar.Hebrew.calendarDate 30 5781 HebrewMonths.Kislev
