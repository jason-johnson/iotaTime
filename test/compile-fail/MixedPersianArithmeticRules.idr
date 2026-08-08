module MixedPersianArithmeticRules
-- EXPECT: Mismatch between: Simple and Birashk

import IotaTime

invalid : CalendarDate PersianArithmetic
invalid = IotaTime.Calendar.Persian.simpleCalendarDate 1 PersianMonths.Farvardin 1404