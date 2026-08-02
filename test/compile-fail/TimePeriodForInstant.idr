module TimePeriodForInstant
-- EXPECT: Can't find an implementation for HasTime Instant.

import IotaTime

invalid : Period Instant
invalid = minutes 1