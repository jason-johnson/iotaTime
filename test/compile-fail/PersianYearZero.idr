module PersianYearZero
-- EXPECT: Can't find an implementation for So (

import IotaTime
import IotaTime.Calendar.Persian

invalid : CalendarDate Persian
invalid = IotaTime.Calendar.Persian.calendarDate 1 PersianMonths.Farvardin 0
