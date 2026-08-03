module IotaTime.Locale.Unix.Platform

import Data.Vect

%default total

%foreign "C:iotatime_unix_locale_snapshot, libiotatime_unix"
prim__unixLocaleSnapshot : String -> PrimIO AnyPtr

%foreign "C:iotatime_unix_locale_item, libiotatime_unix"
prim__unixLocaleItem : AnyPtr -> Int -> String

%foreign "C:iotatime_unix_locale_free, libiotatime_unix"
prim__unixLocaleFree : AnyPtr -> PrimIO ()

export
record UnixLocaleData where
  constructor MkUnixLocaleData
  unixMonthNames : Vect 12 String
  unixMonthNamesShort : Vect 12 String
  unixDayNames : Vect 7 String
  unixDayNamesShort : Vect 7 String
  unixAmName : String
  unixPmName : String
  unixRawDateFormat : String
  unixRawTimeFormat : String
  unixRawDateTimeFormat : String

export
localeMonthNames : UnixLocaleData -> Vect 12 String
localeMonthNames = unixMonthNames

export
localeMonthNamesShort : UnixLocaleData -> Vect 12 String
localeMonthNamesShort = unixMonthNamesShort

export
localeDayNames : UnixLocaleData -> Vect 7 String
localeDayNames = unixDayNames

export
localeDayNamesShort : UnixLocaleData -> Vect 7 String
localeDayNamesShort = unixDayNamesShort

export
localeAmName : UnixLocaleData -> String
localeAmName = unixAmName

export
localePmName : UnixLocaleData -> String
localePmName = unixPmName

export
localeDateFormat : UnixLocaleData -> String
localeDateFormat = unixRawDateFormat

export
localeTimeFormat : UnixLocaleData -> String
localeTimeFormat = unixRawTimeFormat

export
localeDateTimeFormat : UnixLocaleData -> String
localeDateTimeFormat = unixRawDateTimeFormat

items : (count : Nat) -> Int -> AnyPtr -> Vect count String
items Z offset pointer = []
items (S count) offset pointer =
  prim__unixLocaleItem pointer offset :: items count (offset + 1) pointer

export
loadUnixLocaleData : String -> IO (Maybe UnixLocaleData)
loadUnixLocaleData name = do
  pointer <- primIO (prim__unixLocaleSnapshot name)
  if prim__nullAnyPtr pointer /= 0
    then pure Nothing
    else do
      let value = MkUnixLocaleData
            (items 12 0 pointer)
            (items 12 12 pointer)
            (items 7 24 pointer)
            (items 7 31 pointer)
            (prim__unixLocaleItem pointer 38)
            (prim__unixLocaleItem pointer 39)
            (prim__unixLocaleItem pointer 40)
            (prim__unixLocaleItem pointer 41)
            (prim__unixLocaleItem pointer 42)
      primIO (prim__unixLocaleFree pointer)
      pure (Just value)