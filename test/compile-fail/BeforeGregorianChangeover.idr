module BeforeGregorianChangeover
-- EXPECT: Can't find an implementation for So (

import IotaTime

beforeGregorianChangeover : CalendarDate Gregorian
beforeGregorianChangeover = IotaTime.Calendar.Gregorian.calendarDate 14 October 1582
