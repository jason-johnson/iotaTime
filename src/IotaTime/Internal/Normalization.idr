module IotaTime.Internal.Normalization

%default total

||| Preserve a value at runtime while preventing importing modules from
||| unfolding an expensive computation during dependent type elaboration.
export
normalizationBarrier : (input -> output) -> input -> output
normalizationBarrier function value = function value
