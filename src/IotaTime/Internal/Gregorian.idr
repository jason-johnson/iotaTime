module IotaTime.Internal.Gregorian

%default total

yearsPerLeapCycle : Integer
yearsPerLeapCycle = 4

yearsPerCentury : Integer
yearsPerCentury = 100

yearsPerEra : Integer
yearsPerEra = 4 * yearsPerCentury

daysPerCommonYear : Integer
daysPerCommonYear = 365

leapDaysPerEra : Integer
leapDaysPerEra = yearsPerEra `div` yearsPerLeapCycle -
  yearsPerEra `div` yearsPerCentury + 1

daysPerEra : Integer
daysPerEra = yearsPerEra * daysPerCommonYear + leapDaysPerEra

daysPerFourCommonYears : Integer
daysPerFourCommonYears = yearsPerLeapCycle * daysPerCommonYear

daysPerCentury : Integer
daysPerCentury = yearsPerCentury * daysPerCommonYear +
  yearsPerCentury `div` yearsPerLeapCycle - 1

erasBeforeEpoch : Integer
erasBeforeEpoch = 5

-- Day zero is March 1, 2000, after five complete 400-year Gregorian eras.
daysBeforeEpoch : Integer
daysBeforeEpoch = erasBeforeEpoch * daysPerEra

monthsPerMarchCycle : Integer
monthsPerMarchCycle = 5

monthsPerYear : Integer
monthsPerYear = 12

marchMonthNumber : Integer
marchMonthNumber = 3

monthsBeforeMarch : Integer
monthsBeforeMarch = marchMonthNumber - 1

monthsFromMarchThroughDecember : Integer
monthsFromMarchThroughDecember = monthsPerYear - monthsBeforeMarch

-- A March-based five-month block has three 31-day and two 30-day months.
daysPerMarchCycle : Integer
daysPerMarchCycle = 3 * 31 + 2 * 30

monthCalculationOffset : Integer
monthCalculationOffset = 2

public export %inline
gregorianDaysFromCivil : Integer -> Integer -> Integer -> Integer
gregorianDaysFromCivil year month day =
  let shiftedYear = if month <= monthsBeforeMarch then year - 1 else year
      era = shiftedYear `div` yearsPerEra
      yearOfEra = shiftedYear - era * yearsPerEra
      shiftedMonth = month + if month > monthsBeforeMarch
        then -marchMonthNumber
        else monthsPerYear - marchMonthNumber
      dayOfYear = (daysPerMarchCycle * shiftedMonth + monthCalculationOffset)
        `div` monthsPerMarchCycle + day - 1
      dayOfEra = yearOfEra * daysPerCommonYear +
        yearOfEra `div` yearsPerLeapCycle -
        yearOfEra `div` yearsPerCentury + dayOfYear
   in era * daysPerEra + dayOfEra - daysBeforeEpoch

public export %inline
gregorianCivilFromDays : Integer -> (Integer, Integer, Integer)
gregorianCivilFromDays value =
  let shifted = value + daysBeforeEpoch
      era = shifted `div` daysPerEra
      dayOfEra = shifted - era * daysPerEra
      yearOfEra =
        (dayOfEra - dayOfEra `div` daysPerFourCommonYears +
          dayOfEra `div` daysPerCentury -
          dayOfEra `div` (daysPerEra - 1)) `div` daysPerCommonYear
      partialYear = yearOfEra + era * yearsPerEra
      dayOfYear = dayOfEra -
        (daysPerCommonYear * yearOfEra +
          yearOfEra `div` yearsPerLeapCycle -
          yearOfEra `div` yearsPerCentury)
      shiftedMonth = (monthsPerMarchCycle * dayOfYear + monthCalculationOffset)
        `div` daysPerMarchCycle
      day = dayOfYear -
        (daysPerMarchCycle * shiftedMonth + monthCalculationOffset)
          `div` monthsPerMarchCycle + 1
      month = shiftedMonth + if shiftedMonth < monthsFromMarchThroughDecember
        then marchMonthNumber
        else -(monthsPerYear - marchMonthNumber)
      year = partialYear + if month <= monthsBeforeMarch then 1 else 0
   in (year, month, day)
