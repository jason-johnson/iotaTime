module IotaTime

public export
record Instant where
  constructor MkInstant
  ticks : Integer

export
epoch : Instant
epoch = MkInstant 0
