module IslamicSafarDayThirty
-- EXPECT: Can't find an implementation for So (

import IotaTime
import IotaTime.Calendar.Islamic

invalid : CalendarDate IslamicBcl
invalid = IotaTime.Calendar.Islamic.calendarDate 30 IslamicMonths.Safar 1443
