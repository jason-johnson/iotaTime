module IotaTime.Internal.Text

import Data.String

%default total

export
padIntegerWith : Char -> Nat -> Integer -> String
padIntegerWith fill width value = padLeft width fill (show value)

export
zeroPadInteger : Nat -> Integer -> String
zeroPadInteger = padIntegerWith '0'