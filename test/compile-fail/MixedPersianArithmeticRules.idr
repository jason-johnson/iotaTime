module MixedPersianArithmeticRules
-- EXPECT: Mismatch between: Simple and Birashk

import IotaTime

invalid : CalendarDate PersianArithmetic
invalid = simplePersianDate 1 PersianMonths.Farvardin 1404