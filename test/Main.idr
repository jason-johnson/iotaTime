module Main

import IotaTime

zeroEpoch : epoch = MkInstant 0
zeroEpoch = Refl

main : IO ()
main = putStrLn "iotaTime tests passed"
