module InvalidGregorianDayCount
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalidGregorianDayCount : CalendarDate Gregorian
invalidGregorianDayCount = gregorianFromDays (-152445)
