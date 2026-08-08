module InvalidLeapDay
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalidLeapDay : CalendarDate Gregorian
invalidLeapDay = IotaTime.Calendar.Gregorian.calendarDate 29 February 2021
