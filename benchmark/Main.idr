module Main

import Data.Bits
import Data.Buffer
import IotaTime.Internal.Gregorian
import System.Clock

%default covering

daysPerCentury : Integer
daysPerCentury = 36524

daysPerEra : Integer
daysPerEra = 146097

tableBytes : Int
tableBytes = 2 * cast daysPerCentury

packDate : Integer -> Integer -> Integer -> Bits16
packDate year month day =
  cast ((year - 2000) * 512 + month * 32 + day)

fillPackedTable : Buffer -> Int -> IO ()
fillPackedTable table index =
  if index >= cast daysPerCentury
    then pure ()
    else do
      let (year, month, day) = gregorianCivilFromDays (cast index)
      setBits16 table (2 * index) (packDate year month day)
      fillPackedTable table (index + 1)

%foreign "scheme:blodwen-buffer-getbits16"
         "RefC:getBufferUInt16LE"
         "node:lambda:(buf,offset)=>buf.readUInt16LE(offset)"
prim__packedTableIndex : Buffer -> Int -> Bits16

packedTableIndex : Buffer -> Integer -> Bits16
packedTableIndex table index =
  prim__packedTableIndex table (2 * cast index)

record CachedGregorianDate where
  constructor MkCachedGregorianDate
  cycle : Integer
  century : Integer
  dayOfCentury : Integer

cachedDateFromDays : Integer -> CachedGregorianDate
cachedDateFromDays value =
  let cycle = value `div` daysPerEra
      dayOfCycle = value - cycle * daysPerEra
      century0 = dayOfCycle `div` daysPerCentury
      dayOfCentury0 = dayOfCycle - century0 * daysPerCentury
   in if century0 == 4
        then MkCachedGregorianDate cycle 3
          (dayOfCentury0 + daysPerCentury)
        else MkCachedGregorianDate cycle century0 dayOfCentury0

cachedDateToDays : CachedGregorianDate -> Integer
cachedDateToDays date =
  date.cycle * daysPerEra +
  date.century * daysPerCentury +
  date.dayOfCentury

monthDayOffset : Integer -> Integer
monthDayOffset 0 = 0
monthDayOffset 1 = 31
monthDayOffset 2 = 61
monthDayOffset 3 = 92
monthDayOffset 4 = 122
monthDayOffset 5 = 153
monthDayOffset 6 = 184
monthDayOffset 7 = 214
monthDayOffset 8 = 245
monthDayOffset 9 = 275
monthDayOffset 10 = 306
monthDayOffset _ = 337

cachedDateFromYmd : Integer -> Integer -> Integer -> CachedGregorianDate
cachedDateFromYmd year month day =
  let shiftedYears = if month < 3 then year - 2001 else year - 2000
      cycle = shiftedYears `div` 400
      yearInCycle = shiftedYears `mod` 400
      century = yearInCycle `div` 100
      yearOfCentury = yearInCycle `mod` 100
      shiftedMonth = if month > 2 then month - 3 else month + 9
      dayOfCentury = yearOfCentury * 365 + yearOfCentury `div` 4 +
        monthDayOffset shiftedMonth + day - 1
   in MkCachedGregorianDate cycle century dayOfCentury

cachedDateToYmd : Buffer -> CachedGregorianDate ->
                  (Integer, Integer, Integer)
cachedDateToYmd table date =
  if date.dayOfCentury == daysPerCentury
    then (2000 + (date.cycle + 1) * 400, 2, 29)
    else
      let packed = packedTableIndex table date.dayOfCentury
          year = cast (packed `shiftR` 9)
          month = cast ((packed `shiftR` 5) .&. 15)
          day = cast (packed .&. 31)
       in (2000 + date.cycle * 400 + date.century * 100 + year,
            month, day)

verifyCachedRepresentation : Buffer -> Integer -> Integer -> IO Bool
verifyCachedRepresentation table day end =
  if day >= end
    then pure True
    else
      let cached = cachedDateToYmd table (cachedDateFromDays day)
       in if cached == gregorianCivilFromDays day
        then verifyCachedRepresentation table (day + 1) end
            else do
              putStrLn ("cached representation mismatch at day " ++
                show day ++ ": " ++
                show cached ++ " /= " ++ show (gregorianCivilFromDays day))
              pure False

nextDay : Integer -> Integer
nextDay day = day + 1

currentProjectionLoop : Int -> Integer -> Integer -> Integer
currentProjectionLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else
      let (year, month, day) = gregorianCivilFromDays current
       in currentProjectionLoop (remaining - 1) (nextDay current)
            (checksum + year + month + day)

cachedConstructionLoop : Buffer -> Int -> Integer -> Integer -> Integer
cachedConstructionLoop table remaining current checksum =
  if remaining <= 0
    then checksum
    else
      let (year, month, day) =
        cachedDateToYmd table (cachedDateFromDays current)
       in cachedConstructionLoop table (remaining - 1) (nextDay current)
            (checksum + year + month + day)

nextCachedDate : CachedGregorianDate -> CachedGregorianDate
nextCachedDate date =
  if date.dayOfCentury < daysPerCentury - 1
    then { dayOfCentury $= (+ 1) } date
    else if date.century < 3
      then MkCachedGregorianDate date.cycle (date.century + 1) 0
      else if date.dayOfCentury == daysPerCentury - 1
        then { dayOfCentury := daysPerCentury } date
        else MkCachedGregorianDate (date.cycle + 1) 0 0

shiftCachedDate : Integer -> CachedGregorianDate -> CachedGregorianDate
shiftCachedDate amount date =
  let shifted = date.dayOfCentury + amount
   in if shifted >= 0 && shifted < daysPerCentury
        then { dayOfCentury := shifted } date
        else cachedDateFromDays (cachedDateToDays date + amount)

cachedProjectionLoop : Buffer -> Int -> CachedGregorianDate -> Integer ->
                       Integer
cachedProjectionLoop table remaining current checksum =
  if remaining <= 0
    then checksum
    else
      let (year, month, day) = cachedDateToYmd table current
       in cachedProjectionLoop table (remaining - 1)
        (nextCachedDate current)
            (checksum + year + month + day)

currentFromDaysLoop : Int -> Integer -> Integer -> Integer
currentFromDaysLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else currentFromDaysLoop (remaining - 1) (nextDay current)
      (checksum + current)

cachedFromDaysLoop : Int -> Integer -> Integer -> Integer
cachedFromDaysLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else
      let date = cachedDateFromDays current
       in cachedFromDaysLoop (remaining - 1) (nextDay current)
            (checksum + date.cycle + date.century + date.dayOfCentury)

currentToDaysLoop : Int -> Integer -> Integer -> Integer
currentToDaysLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else currentToDaysLoop (remaining - 1) (nextDay current)
      (checksum + current)

cachedToDaysLoop : Int -> CachedGregorianDate -> Integer -> Integer
cachedToDaysLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else cachedToDaysLoop (remaining - 1) (nextCachedDate current)
      (checksum + cachedDateToDays current)

currentSmallShiftLoop : Int -> Integer -> Integer -> Integer
currentSmallShiftLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else
      let shifted = current + 7
       in currentSmallShiftLoop (remaining - 1) (nextDay current)
            (checksum + shifted)

cachedSmallShiftLoop : Int -> CachedGregorianDate -> Integer -> Integer
cachedSmallShiftLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else
      let shifted = shiftCachedDate 7 current
       in cachedSmallShiftLoop (remaining - 1) (nextCachedDate current)
            (checksum + cachedDateToDays shifted)

currentWeekdayLoop : Int -> Integer -> Integer -> Integer
currentWeekdayLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else currentWeekdayLoop (remaining - 1) (nextDay current)
      (checksum + (current + 3) `mod` 7)

cachedWeekdayLoop : Int -> CachedGregorianDate -> Integer -> Integer
cachedWeekdayLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else
      let weekday = (5 * current.century + current.dayOfCentury + 3) `mod` 7
       in cachedWeekdayLoop (remaining - 1) (nextCachedDate current)
            (checksum + weekday)

currentYmdConstructionLoop : Int -> Integer -> Integer
currentYmdConstructionLoop remaining checksum =
  if remaining <= 0
    then checksum
    else
      let year = 2000 + cast (remaining `mod` 400)
          month = 1 + cast (remaining `mod` 12)
          day = 1 + cast (remaining `mod` 28)
          value = gregorianDaysFromCivil year month day
       in currentYmdConstructionLoop (remaining - 1) (checksum + value)

cachedYmdConstructionLoop : Int -> Integer -> Integer
cachedYmdConstructionLoop remaining checksum =
  if remaining <= 0
    then checksum
    else
      let year = 2000 + cast (remaining `mod` 400)
          month = 1 + cast (remaining `mod` 12)
          day = 1 + cast (remaining `mod` 28)
          value = cachedDateFromYmd year month day
       in cachedYmdConstructionLoop (remaining - 1)
            (checksum + cachedDateToDays value)

currentComparisonLoop : Int -> Integer -> Integer -> Integer
currentComparisonLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else
      let result = if current < current + 31 then 1 else 0
       in currentComparisonLoop (remaining - 1) (nextDay current)
            (checksum + result)

cachedLessThan : CachedGregorianDate -> CachedGregorianDate -> Bool
cachedLessThan left right =
  left.cycle < right.cycle ||
  (left.cycle == right.cycle &&
    (left.century < right.century ||
      (left.century == right.century &&
        left.dayOfCentury < right.dayOfCentury)))

cachedComparisonLoop : Int -> CachedGregorianDate -> Integer -> Integer
cachedComparisonLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else
      let result = if cachedLessThan current (shiftCachedDate 31 current)
                     then 1 else 0
       in cachedComparisonLoop (remaining - 1) (nextCachedDate current)
            (checksum + result)

daysInMonth : Integer -> Integer -> Integer
daysInMonth year 2 =
  if year `mod` 400 == 0 ||
    (year `mod` 4 == 0 && year `mod` 100 /= 0) then 29 else 28
daysInMonth _ 4 = 30
daysInMonth _ 6 = 30
daysInMonth _ 9 = 30
daysInMonth _ 11 = 30
daysInMonth _ _ = 31

nextMonthYmd : Integer -> Integer -> Integer ->
               (Integer, Integer, Integer)
nextMonthYmd year month day =
  let targetYear = if month == 12 then year + 1 else year
      targetMonth = if month == 12 then 1 else month + 1
   in (targetYear, targetMonth, min day (daysInMonth targetYear targetMonth))

currentMonthShiftLoop : Int -> Integer -> Integer -> Integer
currentMonthShiftLoop remaining current checksum =
  if remaining <= 0
    then checksum
    else
      let (year, month, day) = gregorianCivilFromDays current
          (targetYear, targetMonth, targetDay) = nextMonthYmd year month day
          shifted = gregorianDaysFromCivil targetYear targetMonth targetDay
       in currentMonthShiftLoop (remaining - 1) (nextDay current)
            (checksum + shifted)

cachedMonthShiftLoop : Buffer -> Int -> CachedGregorianDate -> Integer ->
                       Integer
cachedMonthShiftLoop table remaining current checksum =
  if remaining <= 0
    then checksum
    else
      let (year, month, day) = cachedDateToYmd table current
          (targetYear, targetMonth, targetDay) = nextMonthYmd year month day
          shifted = cachedDateFromYmd targetYear targetMonth targetDay
       in cachedMonthShiftLoop table (remaining - 1) (nextCachedDate current)
            (checksum + cachedDateToDays shifted)

benchmark : String -> Int -> IO Integer -> IO ()
benchmark label iterations action = do
  start <- clockTime Process
  checksum <- action
  finish <- clockTime Process
  let elapsed = toNano finish - toNano start
  putStrLn (label ++ ": " ++ show elapsed ++ " ns total, " ++
    show (elapsed `div` cast iterations) ++ " ns/op, checksum " ++
    show checksum)

iterations : Int
iterations = 5000000

operationIterations : Int
operationIterations = 2000000

main : IO ()
main = do
  Just table <- newBuffer tableBytes
    | Nothing => putStrLn "unable to allocate packed Gregorian table"
  fillPackedTable table 0
  valid <- verifyCachedRepresentation table (-daysPerEra) (2 * daysPerEra)
  if valid
    then do
      putStrLn ("verified " ++ show (3 * daysPerEra) ++
        " days across three eras; raw table size: " ++
        show tableBytes ++ " bytes")
      benchmark "current stored flat day: project Y/M/D" iterations
        (pure (currentProjectionLoop iterations (-152444) 0))
      benchmark "packed date: construct from days and project" iterations
        (pure (cachedConstructionLoop table iterations (-152444) 0))
      benchmark "packed date: project stored table index" iterations
        (pure (cachedProjectionLoop table iterations
          (cachedDateFromDays (-152444)) 0))
      benchmark "current date: construct from epoch days" operationIterations
        (pure (currentFromDaysLoop operationIterations (-152444) 0))
      benchmark "packed date: construct from epoch days" operationIterations
        (pure (cachedFromDaysLoop operationIterations (-152444) 0))
      benchmark "current date: convert to epoch days" operationIterations
        (pure (currentToDaysLoop operationIterations (-152444) 0))
      benchmark "packed date: convert to epoch days" operationIterations
        (pure (cachedToDaysLoop operationIterations
          (cachedDateFromDays (-152444)) 0))
      benchmark "current date: construct from Y/M/D" operationIterations
        (pure (currentYmdConstructionLoop operationIterations 0))
      benchmark "packed date: construct from Y/M/D" operationIterations
        (pure (cachedYmdConstructionLoop operationIterations 0))
      benchmark "current date: shift seven days" operationIterations
        (pure (currentSmallShiftLoop operationIterations (-152444) 0))
      benchmark "packed date: shift seven days" operationIterations
        (pure (cachedSmallShiftLoop operationIterations
          (cachedDateFromDays (-152444)) 0))
      benchmark "current date: calculate weekday" operationIterations
        (pure (currentWeekdayLoop operationIterations (-152444) 0))
      benchmark "packed date: calculate weekday" operationIterations
        (pure (cachedWeekdayLoop operationIterations
          (cachedDateFromDays (-152444)) 0))
      benchmark "current date: compare dates" operationIterations
        (pure (currentComparisonLoop operationIterations (-152444) 0))
      benchmark "packed date: compare dates" operationIterations
        (pure (cachedComparisonLoop operationIterations
          (cachedDateFromDays (-152444)) 0))
      benchmark "current date: shift one month" operationIterations
        (pure (currentMonthShiftLoop operationIterations (-152444) 0))
      benchmark "packed date: shift one month" operationIterations
        (pure (cachedMonthShiftLoop table operationIterations
          (cachedDateFromDays (-152444)) 0))
    else pure ()