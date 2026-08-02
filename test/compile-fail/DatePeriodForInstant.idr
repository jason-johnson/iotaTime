module DatePeriodForInstant
-- EXPECT: Can't find an implementation for HasCalendar Instant.

import IotaTime

invalid : Period Instant
invalid = days 1