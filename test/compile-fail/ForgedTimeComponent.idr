-- EXPECT: Undefined name hourFromInteger

module ForgedTimeComponent

import IotaTime

invalid : Hour
invalid = hourFromInteger 99
