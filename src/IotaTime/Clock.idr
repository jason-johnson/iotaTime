module IotaTime.Clock

import System.Clock

%default total

export
currentUnixNanoseconds : IO Integer
currentUnixNanoseconds = do
  current <- clockTime UTC
  pure (toNano current)
