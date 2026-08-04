module Test.Offset

import IotaTime
import Test.Support

positiveOffset : Offset
positiveOffset = IotaTime.Offset.fromSeconds 5445

negativeOffset : Offset
negativeOffset = IotaTime.Offset.fromSeconds (-5445)

offsetCases : List RuntimeCase
offsetCases =
    [ MkRuntimeCase "HodaTime offset constructors retain module-qualified names"
            (IotaTime.Offset.fromHours (-1) == IotaTime.Offset.fromMinutes (-60))
    , MkRuntimeCase "HodaTime offset accessors are sign-consistent"
            ((IotaTime.Offset.hours negativeOffset,
                IotaTime.Offset.minutes negativeOffset,
                IotaTime.Offset.seconds negativeOffset) == (-1, -30, -45))
    , MkRuntimeCase "HodaTime clamped arithmetic names remain available"
            (IotaTime.Offset.addClamped (IotaTime.Offset.fromHours 17)
                (IotaTime.Offset.fromHours 2) == IotaTime.Offset.fromHours 18 &&
             IotaTime.Offset.minusClamped IotaTime.Offset.empty
                (IotaTime.Offset.fromHours 2) == IotaTime.Offset.fromHours (-2))
    , MkRuntimeCase "zero offset has no displacement"
    (empty == IotaTime.Offset.fromSeconds 0)
  , MkRuntimeCase "offset seconds preserve the exact value"
            ((hours positiveOffset, minutes positiveOffset, seconds positiveOffset) ==
                (1, 30, 45))
  , MkRuntimeCase "offset minutes convert to seconds"
    (IotaTime.Offset.fromMinutes (-90) == IotaTime.Offset.fromSeconds (-5400))
  , MkRuntimeCase "offset hours convert to seconds"
    (IotaTime.Offset.fromHours 18 == IotaTime.Offset.fromSeconds 64800)
  , MkRuntimeCase "negative eighteen hours is accepted"
    (IotaTime.Offset.fromHours (-18) == IotaTime.Offset.fromSeconds (-64800))
  , MkRuntimeCase "runtime lower bound is accepted"
            (case refineOffsetSeconds (-64800) of
                Right value => value == IotaTime.Offset.fromHours (-18)
                Left _ => False)
  , MkRuntimeCase "runtime upper bound is accepted"
            (case refineOffsetSeconds 64800 of
                Right value => value == IotaTime.Offset.fromHours 18
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
            ((hours positiveOffset, minutes positiveOffset,
                seconds positiveOffset) == (1, 30, 45))
  , MkRuntimeCase "negative components are sign-consistent"
            ((hours negativeOffset, minutes negativeOffset,
                seconds negativeOffset) == (-1, -30, -45))
  , MkRuntimeCase "sub-hour negative offset has negative minutes"
            ((hours (IotaTime.Offset.fromMinutes (-30)),
                minutes (IotaTime.Offset.fromMinutes (-30))) == (0, -30))
  , MkRuntimeCase "signed components reconstruct total seconds"
            (hours negativeOffset * 3600 +
                minutes negativeOffset * 60 + seconds negativeOffset == -5445)
  , MkRuntimeCase "offset addition clamps at positive bound"
            (addClamped (IotaTime.Offset.fromHours 17) (IotaTime.Offset.fromHours 2) ==
                IotaTime.Offset.fromHours 18)
  , MkRuntimeCase "offset addition clamps at negative bound"
            (addClamped (IotaTime.Offset.fromHours (-17))
                (IotaTime.Offset.fromHours (-2)) == IotaTime.Offset.fromHours (-18))
  , MkRuntimeCase "offset subtraction clamps at negative bound"
            (minusClamped (IotaTime.Offset.fromHours (-17))
                (IotaTime.Offset.fromHours 2) == IotaTime.Offset.fromHours (-18))
  , MkRuntimeCase "offset subtraction clamps at positive bound"
            (minusClamped (IotaTime.Offset.fromHours 17)
                (IotaTime.Offset.fromHours (-2)) == IotaTime.Offset.fromHours 18)
  , MkRuntimeCase "offset ordering follows total seconds"
            (IotaTime.Offset.fromMinutes (-30) < empty &&
                empty < IotaTime.Offset.fromMinutes 30)
  ]

export
run : IO Bool
run = runSuite "offset tests" offsetCases
