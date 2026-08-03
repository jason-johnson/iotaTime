module PersianMehrDayThirtyOne
-- EXPECT: Can't find an implementation for So (

import IotaTime
import IotaTime.Calendar.Persian

invalid : CalendarDate Persian
invalid = persianDate 31 PersianMonths.Mehr 1400
