module AbsentFifthHebrewWeekday
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate HebrewCivil
invalid = hebrewFromNthDay Fifth HebrewWeekdays.Monday 5784 HebrewMonths.Tishri
