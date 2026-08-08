module Test.Pattern

import Data.Vect
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

isWeekdayMismatch : Either PatternError value -> Bool
isWeekdayMismatch (Left (InvalidValue "weekday does not match date")) = True
isWeekdayMismatch _ = False

isTrailingInputAt : Integer -> Either PatternError value -> Bool
isTrailingInputAt expected (Left (TrailingInput actual _)) = actual == expected
isTrailingInputAt _ _ = False

parsesAs : Pattern DateFields (CalendarDate Gregorian) -> String ->
           CalendarDate Gregorian -> Bool
parsesAs pattern source expected = case IotaTime.Pattern.parse pattern source of
  Left _ => False
  Right actual => calendarDays actual == calendarDays expected

unpadded : Pattern DateFields (CalendarDate Gregorian)
unpadded = ((pyear {calendar = Gregorian} 1 <% char '-') <+>
    (pmonthNum {calendar = Gregorian} 1 <% char '-')) <+>
    pday {calendar = Gregorian} 1

reordered : Pattern DateFields (CalendarDate Gregorian)
reordered = ((pday {calendar = Gregorian} 1 <% char '/') <+>
    (pmonthNum {calendar = Gregorian} 1 <% char '/')) <+>
    pyear {calendar = Gregorian} 1

twoDigitYear : Pattern DateFields (CalendarDate Gregorian)
twoDigitYear = ((pyy {calendar = Gregorian} <% char '-') <+>
    (pMM {calendar = Gregorian} <% char '-')) <+> pdd {calendar = Gregorian}

namedDate : Pattern DateFields (CalendarDate Gregorian)
namedDate = ((pdd {calendar = Gregorian} <% char ' ') <+>
    (pMMM {calendar = Gregorian} <% char ' ')) <+>
    pyyyy {calendar = Gregorian}

weekdayDate : Pattern DateFields (CalendarDate Gregorian)
weekdayDate = (((pdddd {calendar = Gregorian} <% string ", ") <+>
    (pdd {calendar = Gregorian} <% char ' ')) <+>
    (pMMMM {calendar = Gregorian} <% char ' ')) <+>
    pyyyy {calendar = Gregorian}

verifiedWeekdayDate : Pattern DateFields (CalendarDate Gregorian)
verifiedWeekdayDate = (((pddddVerified {calendar = Gregorian} <% string ", ") <+>
    (pdd {calendar = Gregorian} <% char ' ')) <+>
    (pMMMM {calendar = Gregorian} <% char ' ')) <+>
    pyyyy {calendar = Gregorian}

spaceDate : Pattern DateFields (CalendarDate Gregorian)
spaceDate = ((pdaySpace {calendar = Gregorian} <% char '.') <+>
    (pMM {calendar = Gregorian} <% char '.')) <+>
    pyyyy {calendar = Gregorian}

customMonthNames : Vect 12 String
customMonthNames =
    [ "JanX", "FebX", "MarX", "AprX", "MayX", "JunX"
    , "JulX", "AugX", "SepX", "OctX", "NovX", "DecX"
    ]

customWeekdayNames : Vect 7 String
customWeekdayNames =
    [ "SunX", "MonX", "TueX", "WedX", "ThuX", "FriX", "SatX" ]

repeatedMonthNames : Vect 12 String
repeatedMonthNames =
    [ "Odd", "Even", "Odd", "Even", "Odd", "Even"
    , "Odd", "Even", "Odd", "Even", "Odd", "Even"
    ]

overlappingMonthNames : Vect 12 String
overlappingMonthNames =
    [ "Mar", "March", "M03", "M04", "M05", "M06"
    , "M07", "M08", "M09", "M10", "M11", "M12"
    ]

emptyMonthNames : Vect 12 String
emptyMonthNames =
    [ "", "Named", "M03", "M04", "M05", "M06"
    , "M07", "M08", "M09", "M10", "M11", "M12"
    ]

patternCases : List RuntimeCase
patternCases =
  [ MkRuntimeCase "pR formats an ISO Gregorian date"
            (IotaTime.Pattern.format (pR {calendar = Gregorian})
                (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) == "2020-03-03")
  , MkRuntimeCase "pR parses an ISO Gregorian date"
            (parsesAs (pR {calendar = Gregorian}) "2020-03-03"
                (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020))
  , MkRuntimeCase "custom numeric fields omit padding at width one"
      (IotaTime.Pattern.format unpadded (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) == "2020-3-3" &&
       parsesAs unpadded "2020-3-3" (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020))
  , MkRuntimeCase "width-one fields accept padded input"
      (parsesAs unpadded "2020-03-03" (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020))
  , MkRuntimeCase "pyy formats and infers the nearest century"
    (IotaTime.Pattern.format (pyy {calendar = Gregorian})
      (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) == "20" &&
       parsesAs twoDigitYear "20-03-03" (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) &&
       parsesAs twoDigitYear "99-03-03" (IotaTime.Calendar.Gregorian.calendarDate 3 March 1999))
  , MkRuntimeCase "field order is independent of date validation"
      (parsesAs reordered "3/3/2020" (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020))
    , MkRuntimeCase "English month names parse case-insensitively"
            (IotaTime.Pattern.format (pMMM {calendar = Gregorian})
                (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) == "Mar" &&
             IotaTime.Pattern.format (pMMMM {calendar = Gregorian})
                (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) == "March" &&
             parsesAs namedDate "03 mAr 2020" (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020))
    , MkRuntimeCase "weekday names format and consume without validation"
            (IotaTime.Pattern.format (pddd {calendar = Gregorian})
                (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) == "Tue" &&
             IotaTime.Pattern.format (pdddd {calendar = Gregorian})
                (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) == "Tuesday" &&
             parsesAs weekdayDate "Monday, 03 March 2020"
                 (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020))
    , MkRuntimeCase "verified weekday names reject inconsistent dates"
            (IotaTime.Pattern.format verifiedWeekdayDate
                 (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) == "Tuesday, 03 March 2020" &&
             parsesAs verifiedWeekdayDate "Tuesday, 03 March 2020"
                 (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) &&
             isWeekdayMismatch (IotaTime.Pattern.parse verifiedWeekdayDate
                 "Monday, 03 March 2020"))
    , MkRuntimeCase "pD formats the English long date"
            (IotaTime.Pattern.format (pD {calendar = Gregorian})
                (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) ==
                "Tuesday, 03 March 2020")
    , MkRuntimeCase "partial named date patterns use default fields"
                (IotaTime.Pattern.format (pmonthDay {calendar = Gregorian})
                     (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) ==
                "March 03" &&
                 parsesAs (pmonthDay {calendar = Gregorian}) "March 03"
                     (IotaTime.Calendar.Gregorian.calendarDate 3 March 2000) &&
                 IotaTime.Pattern.format (pyearMonth {calendar = Gregorian})
                     (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) ==
                "2020 March" &&
                 parsesAs (pyearMonth {calendar = Gregorian}) "2020 March"
                     (IotaTime.Calendar.Gregorian.calendarDate 1 March 2020))
        , MkRuntimeCase "seeded parsing supplies omitted date fields"
                        (case IotaTime.Pattern.parseWith
                                (pmonthDay {calendar = Gregorian})
                                (MkDateFields 2024 1 1) "March 03" of
                            Right actual => calendarDays actual ==
                                calendarDays (IotaTime.Calendar.Gregorian.calendarDate 3 March 2024)
                            Left _ => False)
    , MkRuntimeCase "pdaySpace formats padding and accepts common forms"
            (IotaTime.Pattern.format (pdaySpace {calendar = Gregorian})
                (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) == " 3" &&
             parsesAs spaceDate " 3.03.2020" (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) &&
             parsesAs spaceDate "3.03.2020" (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) &&
             parsesAs spaceDate "03.03.2020" (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020))
    , MkRuntimeCase "custom name tables are bidirectional"
            (IotaTime.Pattern.format (pMonthName customMonthNames)
                (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) == "MarX" &&
             IotaTime.Pattern.format (pDayName customWeekdayNames)
                (IotaTime.Calendar.Gregorian.calendarDate 3 March 2020) == "TueX")
    , MkRuntimeCase "duplicate custom names select the first matching month"
            (IotaTime.Pattern.format (pMonthName repeatedMonthNames)
                    (IotaTime.Calendar.Gregorian.calendarDate 1 April 2000) == "Even" &&
             parsesAs (pMonthName repeatedMonthNames) "Even"
                 (IotaTime.Calendar.Gregorian.calendarDate 1 February 2000))
    , MkRuntimeCase "custom names parse longest prefixes first"
            (parsesAs (pMonthName overlappingMonthNames) "March"
                (IotaTime.Calendar.Gregorian.calendarDate 1 February 2000))
    , MkRuntimeCase "empty custom names are excluded from parsing"
            (IotaTime.Pattern.format (pMonthName emptyMonthNames)
                    (IotaTime.Calendar.Gregorian.calendarDate 1 January 2000) == "" &&
             parsesAs (pMonthName emptyMonthNames) "Named"
                 (IotaTime.Calendar.Gregorian.calendarDate 1 February 2000))
  , MkRuntimeCase "patterns reject trailing input"
            (isTrailingInputAt 10 (IotaTime.Pattern.parse
                (pR {calendar = Gregorian}) "2020-03-03Z"))
  , MkRuntimeCase "patterns report an incomplete field position"
            (isUnexpectedEndAt 5 (IotaTime.Pattern.parse
                (pR {calendar = Gregorian}) "2020-"))
  , MkRuntimeCase "patterns reject incorrect literals"
      (isUnexpectedCharacterAt 4 '/'
                (IotaTime.Pattern.parse (pR {calendar = Gregorian}) "2020/03/03"))
  , MkRuntimeCase "patterns reject nonnumeric fields"
            (isLeft (IotaTime.Pattern.parse
                (pR {calendar = Gregorian}) "2020-MM-03"))
  , MkRuntimeCase "numeric range errors retain their field position"
            (isMonthRangeError (IotaTime.Pattern.parse
                (pR {calendar = Gregorian}) "2020-13-03"))
  , MkRuntimeCase "final refinement rejects invalid Gregorian dates"
            (isLeft (IotaTime.Pattern.parse
                (pR {calendar = Gregorian}) "2021-02-29"))
  ]

export
run : IO Bool
run = runSuite "calendar date pattern tests" patternCases
