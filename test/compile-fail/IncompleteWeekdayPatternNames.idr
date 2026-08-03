module IncompleteWeekdayPatternNames
-- EXPECT: Vect 6 String and: Vect 7 String

import Data.Vect
import IotaTime

names : Vect 6 String
names =
  [ "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday" ]

invalid : Pattern DateFields (CalendarDate Gregorian)
invalid = pDayName names
