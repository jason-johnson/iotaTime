-- EXPECT: Name IotaTime.Pattern.MkPattern is private.

module ForgedPattern

import IotaTime

invalid : Pattern Integer Integer
invalid = MkPattern
