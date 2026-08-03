module Test.PatternLocalTime

import IotaTime
import Test.Support

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

parsesAs : Pattern TimeFields LocalTime -> String -> LocalTime -> Bool
parsesAs pattern source expected = case IotaTime.Pattern.parse pattern source of
  Left _ => False
  Right actual => actual == expected

hhThenPeriod : Pattern TimeFields LocalTime
hhThenPeriod = ((phh <% char ':') <+> (pmm <% char ' ')) <+> ppp

periodThenHh : Pattern TimeFields LocalTime
periodThenHh = (ppp <% char ' ') <+> ((phh <% char ':') <+> pmm)

spaceHour : Pattern TimeFields LocalTime
spaceHour = (phhSpace <% char ':') <+> pmm

localePeriod : Pattern TimeFields LocalTime
localePeriod = ((phh <% char ':') <+> (pmm <% char ' ')) <+> ppp' jaJP

patternLocalTimeCases : List RuntimeCase
patternLocalTimeCases =
  [ MkRuntimeCase "24-hour fields format with configured width"
      (IotaTime.Pattern.format pHH (localTime 3 4 5 0) == "03" &&
       IotaTime.Pattern.format (phour 1) (localTime 3 4 5 0) == "3" &&
       IotaTime.Pattern.format pmm (localTime 3 4 5 0) == "04" &&
       IotaTime.Pattern.format pss (localTime 3 4 5 0) == "05")
  , MkRuntimeCase "24-hour fields parse padded and unpadded forms"
      (parsesAs pT "03:04:05" (localTime 3 4 5 0) &&
       parsesAs ((phour 1 <% char ':') <+> (pminute 1 <% char ':') <+>
         psecond 1) "3:4:5" (localTime 3 4 5 0))
  , MkRuntimeCase "standard time patterns format expected layouts"
      (IotaTime.Pattern.format pt (localTime 13 24 35 0) == "13:24" &&
       IotaTime.Pattern.format pT (localTime 13 24 35 0) == "13:24:35")
  , MkRuntimeCase "12-hour formatting folds midnight noon and afternoon"
      (IotaTime.Pattern.format phh (localTime 0 0 0 0) == "12" &&
       IotaTime.Pattern.format phh (localTime 12 0 0 0) == "12" &&
       IotaTime.Pattern.format phh (localTime 15 0 0 0) == "03")
  , MkRuntimeCase "12-hour and period parsing is order independent"
      (parsesAs hhThenPeriod "03:04 PM" (localTime 15 4 0 0) &&
       parsesAs periodThenHh "PM 03:04" (localTime 15 4 0 0) &&
       parsesAs hhThenPeriod "12:00 AM" (localTime 0 0 0 0) &&
       parsesAs periodThenHh "AM 12:00" (localTime 0 0 0 0))
  , MkRuntimeCase "short and long period patterns format correctly"
      (IotaTime.Pattern.format pp (localTime 3 0 0 0) == "A" &&
       IotaTime.Pattern.format pp (localTime 15 0 0 0) == "P" &&
       IotaTime.Pattern.format ppp (localTime 12 0 0 0) == "PM")
  , MkRuntimeCase "locale period names are bidirectional"
      (IotaTime.Pattern.format (ppp' jaJP) (localTime 15 0 0 0) == "午後" &&
       parsesAs localePeriod "03:04 午後" (localTime 15 4 0 0))
  , MkRuntimeCase "space-padded 12-hour fields accept padded and bare forms"
      (IotaTime.Pattern.format phhSpace (localTime 13 0 0 0) == " 1" &&
       parsesAs spaceHour " 1:24" (localTime 1 24 0 0) &&
       parsesAs spaceHour "1:24" (localTime 1 24 0 0))
  , MkRuntimeCase "fraction patterns scale to nanoseconds"
      (IotaTime.Pattern.format (pfrac 3) (localTime 0 0 0 123456789) == "123" &&
       IotaTime.Pattern.format (pfrac 6) (localTime 0 0 0 123456789) == "123456" &&
       parsesAs (pfrac 3) "123" (localTime 0 0 0 123000000) &&
       parsesAs (pfrac 9) "123456789" (localTime 0 0 0 123456789))
  , MkRuntimeCase "round-trip time preserves nanosecond precision"
      (let value = localTime 13 24 35 123456789 in
      IotaTime.Pattern.format pr value == "13:24:35.123456789" &&
      parsesAs pr "13:24:35.123456789" value)
  , MkRuntimeCase "time patterns reject invalid fields"
      (isLeft (IotaTime.Pattern.parse pT "24:00:00") &&
       isLeft (IotaTime.Pattern.parse pT "23:60:00") &&
       isLeft (IotaTime.Pattern.parse phh "00"))
  ]

export
run : IO Bool
run = runSuite "local time pattern tests" patternLocalTimeCases
