module PersianCommonYearEsfandDay
-- EXPECT: Can't find an implementation for So (

import IotaTime
import IotaTime.Calendar.Persian

invalid : CalendarDate Persian
invalid = persianDate 30 PersianMonths.Esfand 1400
