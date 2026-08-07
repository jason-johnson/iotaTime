module IncompleteMonthPatternNames
-- EXPECT: Can't find an implementation for 11 = 12.

import Data.Vect
import IotaTime

names : Vect 11 String
names =
  [ "January", "February", "March", "April", "May", "June"
  , "July", "August", "September", "October", "November"
  ]

invalid : Pattern DateFields (CalendarDate Gregorian)
invalid = pMonthName names
