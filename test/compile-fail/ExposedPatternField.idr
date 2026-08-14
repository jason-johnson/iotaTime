-- EXPECT: Name IotaTime.Pattern.PatternRep.(.initialState) is private.

module ExposedPatternField

import IotaTime

invalid : Pattern state value -> state
invalid pattern = pattern.initialState
