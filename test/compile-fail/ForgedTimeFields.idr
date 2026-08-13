-- EXPECT: Name IotaTime.Pattern.LocalTime.MkTimeFields is private.

module ForgedTimeFields

import IotaTime

invalid : TimeFields
invalid = MkTimeFields
