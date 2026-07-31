module ForgedWeekNumber
-- EXPECT: MkWeekNumber is private.

import IotaTime

forgedWeek : WeekNumber
forgedWeek = MkWeekNumber 1
