module BeforeJulianIntroduction
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate Julian
invalid = IotaTime.Calendar.Julian.calendarDate 31 JulianMonths.December (-45)