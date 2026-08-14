-- EXPECT: Name IotaTime.Pattern.CalendarDate.MkDateFields is private.

module ForgedDateFields

import IotaTime

invalid : DateFields
invalid = MkDateFields
