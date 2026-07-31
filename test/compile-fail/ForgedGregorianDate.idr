module ForgedGregorianDate
-- EXPECT: MkGregorianDate is private.

import IotaTime

forgedGregorianDate : CalendarDate Gregorian
forgedGregorianDate = MkGregorianDate (-152445)
