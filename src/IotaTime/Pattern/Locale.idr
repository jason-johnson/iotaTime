module IotaTime.Pattern.Locale

import IotaTime.Locale
import IotaTime.Pattern
import IotaTime.Pattern.CalendarDate
import IotaTime.Pattern.LocalTime
import IotaTime.Calendar
import IotaTime.Calendar.Gregorian
import IotaTime.LocalTime

%default total

public export
data StrftimeError
  = UnsupportedSpecifier Char
  | DanglingPercent

public export
Eq StrftimeError where
  UnsupportedSpecifier left == UnsupportedSpecifier right = left == right
  DanglingPercent == DanglingPercent = True
  _ == _ = False

public export
Show StrftimeError where
  show (UnsupportedSpecifier value) =
    "unsupported strftime specifier: %" ++ pack [value]
  show DanglingPercent = "strftime layout ends with a bare %"

data LayoutToken = LiteralToken Char | ConversionToken Char

data LayoutFragment = LiteralRun String | Conversion Char

compositeTokens : Char -> Maybe (List LayoutToken)
compositeTokens 'T' = Just
  [ ConversionToken 'H', LiteralToken ':', ConversionToken 'M'
  , LiteralToken ':', ConversionToken 'S'
  ]
compositeTokens 'R' = Just
  [ ConversionToken 'H', LiteralToken ':', ConversionToken 'M' ]
compositeTokens 'r' = Just
  [ ConversionToken 'I', LiteralToken ':', ConversionToken 'M'
  , LiteralToken ':', ConversionToken 'S', LiteralToken ' '
  , ConversionToken 'p'
  ]
compositeTokens 'F' = Just
  [ ConversionToken 'Y', LiteralToken '-', ConversionToken 'm'
  , LiteralToken '-', ConversionToken 'd'
  ]
compositeTokens 'D' = Just
  [ ConversionToken 'm', LiteralToken '/', ConversionToken 'd'
  , LiteralToken '/', ConversionToken 'y'
  ]
compositeTokens _ = Nothing

tokenize : List Char -> Either StrftimeError (List LayoutToken)
tokenize [] = Right []
tokenize ['%'] = Left DanglingPercent
tokenize ('%' :: specifier :: rest) = do
  suffix <- tokenize rest
  case specifier of
    '%' => Right (LiteralToken '%' :: suffix)
    'n' => Right (LiteralToken '\n' :: suffix)
    't' => Right (LiteralToken '\t' :: suffix)
    _ => case compositeTokens specifier of
      Just expansion => Right (expansion ++ suffix)
      Nothing => Right (ConversionToken specifier :: suffix)
tokenize (value :: rest) = map (LiteralToken value ::) (tokenize rest)

toFragments : List LayoutToken -> List LayoutFragment
toFragments = foldr step []
  where
    step : LayoutToken -> List LayoutFragment -> List LayoutFragment
    step (LiteralToken value) (LiteralRun text :: rest) =
      LiteralRun (pack [value] ++ text) :: rest
    step (LiteralToken value) rest = LiteralRun (pack [value]) :: rest
    step (ConversionToken value) rest = Conversion value :: rest

dateConversion : Locale -> Char ->
                 Either StrftimeError
                   (Pattern DateFields (CalendarDate Gregorian))
dateConversion locale 'Y' = Right pyyyy
dateConversion locale 'y' = Right pyy
dateConversion locale 'm' = Right pMM
dateConversion locale 'd' = Right pdd
dateConversion locale 'e' = Right pdaySpace
dateConversion locale 'B' = Right (pMMMM' locale)
dateConversion locale 'b' = Right (pMMM' locale)
dateConversion locale 'h' = Right (pMMM' locale)
dateConversion locale 'A' = Right (pdddd' locale)
dateConversion locale 'a' = Right (pddd' locale)
dateConversion _ value = Left (UnsupportedSpecifier value)

fragmentPattern : Pattern state value ->
                  (Char -> Either StrftimeError (Pattern state value)) ->
                  LayoutFragment -> Either StrftimeError (Pattern state value)
fragmentPattern template _ (LiteralRun text) =
  Right (literalField template text)
fragmentPattern _ conversion (Conversion value) = conversion value

assemble : Pattern state value ->
           (Char -> Either StrftimeError (Pattern state value)) ->
           List LayoutFragment -> Either StrftimeError (Pattern state value)
assemble template _ [] = Right (literalField template "")
assemble template conversion [fragment] =
  fragmentPattern template conversion fragment
assemble template conversion (fragment :: rest) = do
  first <- fragmentPattern template conversion fragment
  remaining <- assemble template conversion rest
  Right (first <+> remaining)

public export
compileDatePattern : Locale -> String ->
                     Either StrftimeError
                       (Pattern DateFields (CalendarDate Gregorian))
compileDatePattern locale layout = do
  tokens <- tokenize (unpack layout)
  assemble pyyyy (dateConversion locale) (toFragments tokens)

public export
localeDatePattern : Locale ->
                    Either StrftimeError
                      (Pattern DateFields (CalendarDate Gregorian))
localeDatePattern locale = compileDatePattern locale (rawDateFormat locale)

timeConversion : Locale -> Char ->
                 Either StrftimeError (Pattern TimeFields LocalTime)
timeConversion _ 'H' = Right pHH
timeConversion _ 'I' = Right phh
timeConversion _ 'l' = Right phhSpace
timeConversion _ 'M' = Right pmm
timeConversion _ 'S' = Right pss
timeConversion locale 'p' = Right (ppp' locale)
timeConversion _ value = Left (UnsupportedSpecifier value)

public export
compileTimePattern : Locale -> String ->
                     Either StrftimeError (Pattern TimeFields LocalTime)
compileTimePattern locale layout = do
  tokens <- tokenize (unpack layout)
  assemble pHH (timeConversion locale) (toFragments tokens)

public export
localeTimePattern : Locale ->
                    Either StrftimeError (Pattern TimeFields LocalTime)
localeTimePattern locale = compileTimePattern locale (rawTimeFormat locale)
