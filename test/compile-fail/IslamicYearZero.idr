module IslamicYearZero
-- EXPECT: Can't find an implementation for So (

import IotaTime
import IotaTime.Calendar.Islamic

invalid : CalendarDate IslamicBcl
invalid = islamicDate 1 IslamicMonths.Muharram 0
