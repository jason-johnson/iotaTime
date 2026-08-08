module AbsentFifthWeekday
-- EXPECT: Can't find an implementation for So (

import IotaTime

absentFifthWeekday : CalendarDate Gregorian
absentFifthWeekday = IotaTime.Calendar.Gregorian.fromNthDay
	Fifth Monday February 2000
