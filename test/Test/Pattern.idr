module Test.Pattern

import IotaTime
import Test.Support

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isUnexpectedEndAt : Integer -> Either PatternError value -> Bool
isUnexpectedEndAt expected (Left (UnexpectedEnd actual _)) = actual == expected
isUnexpectedEndAt _ _ = False

isUnexpectedCharacterAt : Integer -> Char -> Either PatternError value -> Bool
isUnexpectedCharacterAt expectedPosition expectedCharacter
        (Left (UnexpectedCharacter actualPosition _ actualCharacter)) =
    actualPosition == expectedPosition && actualCharacter == expectedCharacter
isUnexpectedCharacterAt _ _ _ = False

isMonthRangeError : Either PatternError value -> Bool
isMonthRangeError (Left (ValueOutOfRange "13" 1 12 5)) = True
isMonthRangeError _ = False

isTrailingInputAt : Integer -> Either PatternError value -> Bool
isTrailingInputAt expected (Left (TrailingInput actual _)) = actual == expected
isTrailingInputAt _ _ = False

parsesAs : Pattern DateFields (CalendarDate Gregorian) -> String ->
           CalendarDate Gregorian -> Bool
parsesAs pattern source expected = case IotaTime.Pattern.parse pattern source of
  Left _ => False
  Right actual => calendarDays actual == calendarDays expected

unpadded : Pattern DateFields (CalendarDate Gregorian)
unpadded = ((pyear 1 <% char '-') <+> (pmonthNum 1 <% char '-')) <+> pday 1

reordered : Pattern DateFields (CalendarDate Gregorian)
reordered = ((pday 1 <% char '/') <+> (pmonthNum 1 <% char '/')) <+> pyear 1

twoDigitYear : Pattern DateFields (CalendarDate Gregorian)
twoDigitYear = ((pyy <% char '-') <+> (pMM <% char '-')) <+> pdd

patternCases : List RuntimeCase
patternCases =
  [ MkRuntimeCase "pR formats an ISO Gregorian date"
      (IotaTime.Pattern.format pR (calendarDate 3 March 2020) == "2020-03-03")
  , MkRuntimeCase "pR parses an ISO Gregorian date"
      (parsesAs pR "2020-03-03" (calendarDate 3 March 2020))
  , MkRuntimeCase "custom numeric fields omit padding at width one"
      (IotaTime.Pattern.format unpadded (calendarDate 3 March 2020) == "2020-3-3" &&
       parsesAs unpadded "2020-3-3" (calendarDate 3 March 2020))
  , MkRuntimeCase "width-one fields accept padded input"
      (parsesAs unpadded "2020-03-03" (calendarDate 3 March 2020))
  , MkRuntimeCase "pyy formats and infers the nearest century"
      (IotaTime.Pattern.format pyy (calendarDate 3 March 2020) == "20" &&
       parsesAs twoDigitYear "20-03-03" (calendarDate 3 March 2020) &&
       parsesAs twoDigitYear "99-03-03" (calendarDate 3 March 1999))
  , MkRuntimeCase "field order is independent of date validation"
      (parsesAs reordered "3/3/2020" (calendarDate 3 March 2020))
  , MkRuntimeCase "patterns reject trailing input"
      (isTrailingInputAt 10 (IotaTime.Pattern.parse pR "2020-03-03Z"))
  , MkRuntimeCase "patterns report an incomplete field position"
      (isUnexpectedEndAt 5 (IotaTime.Pattern.parse pR "2020-"))
  , MkRuntimeCase "patterns reject incorrect literals"
      (isUnexpectedCharacterAt 4 '/'
        (IotaTime.Pattern.parse pR "2020/03/03"))
  , MkRuntimeCase "patterns reject nonnumeric fields"
      (isLeft (IotaTime.Pattern.parse pR "2020-MM-03"))
  , MkRuntimeCase "numeric range errors retain their field position"
      (isMonthRangeError (IotaTime.Pattern.parse pR "2020-13-03"))
  , MkRuntimeCase "final refinement rejects invalid Gregorian dates"
      (isLeft (IotaTime.Pattern.parse pR "2021-02-29"))
  ]

export
run : IO Bool
run = runSuite "calendar date pattern tests" patternCases
