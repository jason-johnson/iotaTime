module InvalidGregorianDayCount

import IotaTime

invalidGregorianDayCount : CalendarDate Gregorian
invalidGregorianDayCount = gregorianFromDays (-152445)
