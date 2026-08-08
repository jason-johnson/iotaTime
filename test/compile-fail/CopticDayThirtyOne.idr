module CopticDayThirtyOne
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate Coptic
invalid = IotaTime.Calendar.Coptic.calendarDate 31 CopticMonths.Thout 1738
