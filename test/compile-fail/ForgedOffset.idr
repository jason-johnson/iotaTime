module ForgedOffset
-- EXPECT: IotaTime.Offset.MkOffset is private.

import IotaTime

invalid : Offset
invalid = MkOffset 0
