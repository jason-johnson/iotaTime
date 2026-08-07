module IncompleteCopticMonthPatternNames
-- EXPECT: Can't find an implementation for 12 = 13.

import Data.Vect
import IotaTime

names : Vect 12 String
names =
  [ "M01", "M02", "M03", "M04", "M05", "M06"
  , "M07", "M08", "M09", "M10", "M11", "M12"
  ]

invalid : Pattern DateFields (CalendarDate Coptic)
invalid = pMonthName {calendar = Coptic} names
