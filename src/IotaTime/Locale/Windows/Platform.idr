module IotaTime.Locale.Windows.Platform

import Data.Vect

%default total

%foreign "C:iotatime_windows_locale_snapshot, libiotatime_windows"
prim__windowsLocaleSnapshot : String -> Int -> PrimIO AnyPtr

%foreign "C:iotatime_windows_locale_item, libiotatime_windows"
prim__windowsLocaleItem : AnyPtr -> Int -> String

%foreign "C:iotatime_windows_locale_free, libiotatime_windows"
prim__windowsLocaleFree : AnyPtr -> PrimIO ()

export
record WindowsLocaleData where
  constructor MkWindowsLocaleData
  windowsLocaleId : String
  windowsMonthNames : Vect 12 String
  windowsMonthNamesShort : Vect 12 String
  windowsDayNames : Vect 7 String
  windowsDayNamesShort : Vect 7 String
  windowsAmName : String
  windowsPmName : String
  windowsRawDateFormat : String
  windowsRawTimeFormat : String

export
localeIdentifier : WindowsLocaleData -> String
localeIdentifier = windowsLocaleId

export
localeMonthNames : WindowsLocaleData -> Vect 12 String
localeMonthNames = windowsMonthNames

export
localeMonthNamesShort : WindowsLocaleData -> Vect 12 String
localeMonthNamesShort = windowsMonthNamesShort

export
localeDayNames : WindowsLocaleData -> Vect 7 String
localeDayNames = windowsDayNames

export
localeDayNamesShort : WindowsLocaleData -> Vect 7 String
localeDayNamesShort = windowsDayNamesShort

export
localeAmName : WindowsLocaleData -> String
localeAmName = windowsAmName

export
localePmName : WindowsLocaleData -> String
localePmName = windowsPmName

export
localeDateFormat : WindowsLocaleData -> String
localeDateFormat = windowsRawDateFormat

export
localeTimeFormat : WindowsLocaleData -> String
localeTimeFormat = windowsRawTimeFormat

isPictureField : Char -> Bool
isPictureField value =
  value == 'd' || value == 'M' || value == 'y' || value == 'H' ||
  value == 'h' || value == 'm' || value == 's' || value == 't'

fieldSpecifier : Char -> Nat -> List Char
fieldSpecifier 'd' count = if count >= 4 then unpack "%A"
  else if count == 3 then unpack "%a"
  else if count == 2 then unpack "%d"
  else unpack "%e"
fieldSpecifier 'M' count = if count >= 4 then unpack "%B"
  else if count == 3 then unpack "%b"
  else unpack "%m"
fieldSpecifier 'y' count = if count >= 3 then unpack "%Y" else unpack "%y"
fieldSpecifier 'H' count = unpack "%H"
fieldSpecifier 'h' count = unpack "%I"
fieldSpecifier 'm' count = unpack "%M"
fieldSpecifier 's' count = unpack "%S"
fieldSpecifier 't' count = unpack "%p"
fieldSpecifier value count = []

data PictureMode = PictureText | PictureLiteral

flushField : Maybe (Char, Nat) -> List Char
flushField Nothing = []
flushField (Just (value, count)) = fieldSpecifier value count

translatePicture : PictureMode -> Maybe (Char, Nat) ->
                   List Char -> List Char
translatePicture mode pending [] = flushField pending
translatePicture PictureLiteral pending ('\'' :: '\'' :: rest) =
  flushField pending ++ '\'' :: translatePicture PictureLiteral Nothing rest
translatePicture PictureLiteral pending ('\'' :: rest) =
  flushField pending ++ translatePicture PictureText Nothing rest
translatePicture PictureLiteral pending ('%' :: rest) =
  flushField pending ++ '%' :: '%' ::
    translatePicture PictureLiteral Nothing rest
translatePicture PictureLiteral pending (value :: rest) =
  flushField pending ++ value :: translatePicture PictureLiteral Nothing rest
translatePicture PictureText pending ('\'' :: rest) =
  flushField pending ++ translatePicture PictureLiteral Nothing rest
translatePicture PictureText pending ('%' :: rest) =
  flushField pending ++ '%' :: '%' ::
    translatePicture PictureText Nothing rest
translatePicture PictureText Nothing (value :: rest) =
  if isPictureField value
    then translatePicture PictureText (Just (value, 1)) rest
    else value :: translatePicture PictureText Nothing rest
translatePicture PictureText (Just (field, count)) (value :: rest) =
  if value == field
    then translatePicture PictureText (Just (field, S count)) rest
    else fieldSpecifier field count ++
      if isPictureField value
        then translatePicture PictureText (Just (value, 1)) rest
        else value :: translatePicture PictureText Nothing rest

||| Translate a Windows date/time picture into the supported strftime subset.
export
windowsPictureToStrftime : String -> String
windowsPictureToStrftime =
  pack . translatePicture PictureText Nothing . unpack

items : (count : Nat) -> Int -> AnyPtr -> Vect count String
items Z offset pointer = []
items (S count) offset pointer =
  prim__windowsLocaleItem pointer offset :: items count (offset + 1) pointer

export
loadWindowsLocaleData : Bool -> String -> IO (Maybe WindowsLocaleData)
loadWindowsLocaleData current name = do
  pointer <- primIO (prim__windowsLocaleSnapshot name (if current then 1 else 0))
  if prim__nullAnyPtr pointer /= 0
    then pure Nothing
    else do
      let value = MkWindowsLocaleData
            (prim__windowsLocaleItem pointer 0)
            (items 12 1 pointer)
            (items 12 13 pointer)
            (items 7 25 pointer)
            (items 7 32 pointer)
            (prim__windowsLocaleItem pointer 39)
            (prim__windowsLocaleItem pointer 40)
            (windowsPictureToStrftime (prim__windowsLocaleItem pointer 41))
            (windowsPictureToStrftime (prim__windowsLocaleItem pointer 42))
      primIO (prim__windowsLocaleFree pointer)
      pure (Just value)