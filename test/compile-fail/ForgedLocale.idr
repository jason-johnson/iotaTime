module ForgedLocale
-- EXPECT: Name IotaTime.Locale.MkLocale is private.

import Data.Vect
import IotaTime

invalid : Locale
invalid = MkLocale
  "forged"
  (replicate 12 "")
  (replicate 12 "")
  (replicate 7 "")
  (replicate 7 "")
  ""
  ""
  ""
  ""
  ""
