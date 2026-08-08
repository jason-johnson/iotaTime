module AbsentFifthHebrewWeekday
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate HebrewCivil
invalid = IotaTime.Calendar.Hebrew.fromNthDay Fifth HebrewWeekdays.Monday 5784 HebrewMonths.Tishri
