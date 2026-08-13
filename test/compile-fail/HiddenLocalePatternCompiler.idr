-- EXPECT: Name IotaTime.Pattern.Locale.compileDatePattern is private.

module HiddenLocalePatternCompiler

import IotaTime

bad : Either StrftimeError (Pattern DateFields (CalendarDate Gregorian))
bad = compileDatePattern enUS "%Y-%m-%d"