module BeforeGregorianChangeover
-- EXPECT: Can't find an implementation

import IotaTime

beforeGregorianChangeover : CalendarDate Gregorian
beforeGregorianChangeover = calendarDate 14 October 1582
