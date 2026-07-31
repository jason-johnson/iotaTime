module ForgedYear
-- EXPECT: MkYear is private.

import IotaTime

forgedYear : Year
forgedYear = MkYear 2020
