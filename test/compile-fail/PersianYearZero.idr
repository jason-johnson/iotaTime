module PersianYearZero
-- EXPECT: Can't find an implementation for So (

import IotaTime
import IotaTime.Calendar.Persian

invalid : CalendarDate Persian
invalid = persianDate 1 PersianMonths.Farvardin 0
