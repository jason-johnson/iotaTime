module MixedPersianCalendars
-- EXPECT: Mismatch between: ArithmeticPersianDate Birashk and PersianDate

import IotaTime

invalid : CalendarDate Persian
invalid = arithmeticPersianDate 1 PersianMonths.Farvardin 1404