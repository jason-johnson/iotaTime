module IotaTime.Internal.Text

%default total

repeatCharacter : Char -> Nat -> String
repeatCharacter _ Z = ""
repeatCharacter value (S count) = strCons value (repeatCharacter value count)

export
padIntegerWith : Char -> Nat -> Integer -> String
padIntegerWith fill width value =
  let shown = show value
      currentWidth = length (unpack shown)
   in if currentWidth >= width then shown
      else repeatCharacter fill (width `minus` currentWidth) ++ shown

export
zeroPadInteger : Nat -> Integer -> String
zeroPadInteger = padIntegerWith '0'