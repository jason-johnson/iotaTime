module InvalidJulianLeapDay
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate Julian
invalid = IotaTime.Calendar.Julian.calendarDate 29 JulianMonths.February 1901