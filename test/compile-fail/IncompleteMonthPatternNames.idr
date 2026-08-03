module IncompleteMonthPatternNames
-- EXPECT: Vect 11 String and: Vect 12 String

import Data.Vect
import IotaTime

names : Vect 11 String
names =
  [ "January", "February", "March", "April", "May", "June"
  , "July", "August", "September", "October", "November"
  ]

invalid : Pattern DateFields (CalendarDate Gregorian)
invalid = pMonthName names
