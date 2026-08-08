module MixedPersianCalendars
-- EXPECT: Mismatch between: ArithmeticPersianDate Birashk and PersianDate

import IotaTime

invalid : CalendarDate Persian
invalid = IotaTime.Calendar.Persian.arithmeticCalendarDate 1 PersianMonths.Farvardin 1404