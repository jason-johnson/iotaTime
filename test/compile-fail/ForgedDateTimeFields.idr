-- EXPECT: Name IotaTime.Pattern.Locale.MkDateTimeFields is private.

module ForgedDateTimeFields

import IotaTime

invalid : DateTimeFields
invalid = MkDateTimeFields
