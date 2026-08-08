module AbsentReverseCopticWeekday
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate Coptic
invalid = copticFromNthDay FourthToLast CopticWeekdays.Friday
  CopticMonths.PiKogiEnavot 1732
