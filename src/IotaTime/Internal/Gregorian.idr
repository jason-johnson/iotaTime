module IotaTime.Internal.Gregorian

%default total

public export %inline
gregorianDaysFromCivil : Integer -> Integer -> Integer -> Integer
gregorianDaysFromCivil year month day =
  let shiftedYear = if month <= 2 then year - 1 else year
      era = shiftedYear `div` 400
      yearOfEra = shiftedYear - era * 400
      shiftedMonth = month + if month > 2 then -3 else 9
      dayOfYear = (153 * shiftedMonth + 2) `div` 5 + day - 1
      dayOfEra = yearOfEra * 365 + yearOfEra `div` 4 -
        yearOfEra `div` 100 + dayOfYear
   in era * 146097 + dayOfEra - 730485