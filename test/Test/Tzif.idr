module Test.Tzif

import IotaTime
import IotaTime.Tzdb
import IotaTime.Tzdb.Tzif
import Test.Support

zeros : Nat -> List Bits8
zeros Z = []
zeros (S count) = 0 :: zeros count

word32 : Integer -> List Bits8
word32 value =
  [ cast ((value `div` 16777216) `mod` 256)
  , cast ((value `div` 65536) `mod` 256)
  , cast ((value `div` 256) `mod` 256)
  , cast (value `mod` 256)
  ]

header : Bits8 -> Integer -> List Bits8
header version transitionCount =
  [84, 90, 105, 102, version] ++ zeros 15 ++
  word32 0 ++ word32 0 ++ word32 0 ++ word32 transitionCount ++
  word32 1 ++ word32 4

fixedPayload : List Bits8
fixedPayload = word32 0 ++ [0, 0] ++ [85, 84, 67, 0]

fixedPayload64 : List Bits8
fixedPayload64 = zeros 8 ++ [0, 0] ++ [85, 84, 67, 0]

version1Fixed : List Bits8
version1Fixed = header 0 0 ++ fixedPayload

version4Fixed : List Bits8
version4Fixed = header 52 0 ++ fixedPayload ++ header 52 0 ++
  fixedPayload ++ [10, 85, 84, 67, 48, 10]

invalidTypeIndex : List Bits8
invalidTypeIndex = header 0 1 ++ word32 0 ++ [1] ++ fixedPayload

tzifCases : List RuntimeCase
tzifCases =
  [ MkRuntimeCase "TZif v1 fixed zone is decoded"
      (case parseTzif version1Fixed of
        Right value => value.version == Version1 &&
          abbreviation value.initialTransition == "UTC" &&
          null value.transitions && value.posixFooter == Nothing
        Left _ => False)
  , MkRuntimeCase "TZif v4 POSIX footer is retained"
      (case parseTzif version4Fixed of
        Right value => value.version == Version4 &&
          value.posixFooter == Just "UTC0"
        Left _ => False)
  , MkRuntimeCase "TZif transition type indexes are bounds checked"
      (case parseTzif invalidTypeIndex of
        Left (InvalidTransitionTypeIndex 1) => True
        _ => False)
  ]

export
run : IO Bool
run = do
  purePassed <- runSuite "TZif tests" tzifCases
  systemUtc <- loadTzifFile "/usr/share/zoneinfo/UTC"
  let systemPassed = case systemUtc of
        Right value => abbreviation value.initialTransition == "UTC"
        Left _ => False
  putStrLn ("  [" ++ (if systemPassed then "PASS" else "FAIL") ++
    "] system UTC TZif file is decoded")
  pure (purePassed && systemPassed)