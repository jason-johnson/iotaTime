-- EXPECT: Undefined name .providerUtc.

module ExposedTimeZoneProviderField

import IotaTime

invalid : TimeZoneProvider -> IO (Either TzdbError TimeZone)
invalid provider = provider.providerUtc
