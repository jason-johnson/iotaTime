module Test.Offset

import IotaTime
import Test.Support

positiveOffset : Offset
positiveOffset = offsetFromSeconds 5445

negativeOffset : Offset
negativeOffset = offsetFromSeconds (-5445)

offsetCases : List RuntimeCase
offsetCases =
  [ MkRuntimeCase "zero offset has no displacement"
      (totalOffsetSeconds zeroOffset == 0)
  , MkRuntimeCase "offset seconds preserve the exact value"
      (totalOffsetSeconds positiveOffset == 5445)
  , MkRuntimeCase "offset minutes convert to seconds"
      (totalOffsetSeconds (offsetFromMinutes (-90)) == -5400)
  , MkRuntimeCase "offset hours convert to seconds"
      (totalOffsetSeconds (offsetFromHours 18) == 64800)
  , MkRuntimeCase "negative eighteen hours is accepted"
      (totalOffsetSeconds (offsetFromHours (-18)) == -64800)
  , MkRuntimeCase "runtime lower bound is accepted"
            (case refineOffsetSeconds (-64800) of
                Right value => value == offsetFromHours (-18)
                Left _ => False)
  , MkRuntimeCase "runtime upper bound is accepted"
            (case refineOffsetSeconds 64800 of
                Right value => value == offsetFromHours 18
                Left _ => False)
  , MkRuntimeCase "runtime value below lower bound is rejected"
            (case refineOffsetSeconds (-64801) of
                Left (OffsetOutOfRange value) => value == -64801
                Right _ => False)
  , MkRuntimeCase "runtime value above upper bound is rejected"
            (case refineOffsetSeconds 64801 of
                Left (OffsetOutOfRange value) => value == 64801
                Right _ => False)
  , MkRuntimeCase "positive components are sign-consistent"
      ((offsetHours positiveOffset, offsetMinutes positiveOffset,
        offsetSeconds positiveOffset) == (1, 30, 45))
  , MkRuntimeCase "negative components are sign-consistent"
      ((offsetHours negativeOffset, offsetMinutes negativeOffset,
        offsetSeconds negativeOffset) == (-1, -30, -45))
  , MkRuntimeCase "sub-hour negative offset has negative minutes"
      ((offsetHours (offsetFromMinutes (-30)),
        offsetMinutes (offsetFromMinutes (-30))) == (0, -30))
  , MkRuntimeCase "signed components reconstruct total seconds"
      (offsetHours negativeOffset * 3600 +
        offsetMinutes negativeOffset * 60 + offsetSeconds negativeOffset ==
        totalOffsetSeconds negativeOffset)
  , MkRuntimeCase "offset addition clamps at positive bound"
      (addOffsetClamped (offsetFromHours 17) (offsetFromHours 2) ==
        offsetFromHours 18)
  , MkRuntimeCase "offset addition clamps at negative bound"
      (addOffsetClamped (offsetFromHours (-17)) (offsetFromHours (-2)) ==
        offsetFromHours (-18))
  , MkRuntimeCase "offset subtraction clamps at negative bound"
      (subtractOffsetClamped (offsetFromHours (-17)) (offsetFromHours 2) ==
        offsetFromHours (-18))
  , MkRuntimeCase "offset subtraction clamps at positive bound"
      (subtractOffsetClamped (offsetFromHours 17) (offsetFromHours (-2)) ==
        offsetFromHours 18)
  , MkRuntimeCase "offset negation reverses direction exactly"
      (negateOffset negativeOffset == positiveOffset)
  , MkRuntimeCase "offset ordering follows total seconds"
      (offsetFromMinutes (-30) < zeroOffset && zeroOffset < offsetFromMinutes 30)
  ]

export
run : IO Bool
run = runSuite "offset tests" offsetCases
