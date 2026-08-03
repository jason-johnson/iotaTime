module FractionWidthTen
-- EXPECT: Can't find an implementation for So

import IotaTime

invalid : Pattern TimeFields LocalTime
invalid = pfrac 10
