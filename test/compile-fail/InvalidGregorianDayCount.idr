module InvalidGregorianDayCount
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalidGregorianDayCount : CalendarDate Gregorian
invalidGregorianDayCount = IotaTime.Calendar.Gregorian.fromDays (-152445)
