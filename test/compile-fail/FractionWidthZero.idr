module FractionWidthZero
-- EXPECT: Can't find an implementation for So

import IotaTime

invalid : Pattern TimeFields LocalTime
invalid = pfrac 0
