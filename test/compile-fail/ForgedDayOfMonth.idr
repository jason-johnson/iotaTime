module ForgedDayOfMonth
-- EXPECT: MkDayOfMonth is private.

import IotaTime

forgedDay : DayOfMonth
forgedDay = MkDayOfMonth 40
