-- EXPECT: Can't find an implementation for PeriodTarget CustomTarget.

module CustomApplyPeriod

import IotaTime

data CustomTarget = MkCustomTarget

ApplyPeriod CustomTarget where
  applyPeriod _ value = value
