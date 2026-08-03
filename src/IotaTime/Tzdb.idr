module IotaTime.Tzdb

import public IotaTime.Tzdb.Tzif
import public Data.Buffer
import public System.File.Buffer

%default total

public export
data TzdbError
  = TzdbFileError String
  | TzdbParseError TzifError

bufferBytes : Buffer -> IO (List Bits8)
bufferBytes buffer = do
  size <- rawSize buffer
  readBytes (cast size) 0
  where
    readBytes : Nat -> Int -> IO (List Bits8)
    readBytes Z offset = pure []
    readBytes (S count) offset = do
      byte <- getBits8 buffer offset
      remaining <- readBytes count (offset + 1)
      pure (byte :: remaining)

||| Read and decode one TZif file. Filesystem and format failures remain
||| distinct typed trust-boundary errors.
public export
loadTzifFile : String -> IO (Either TzdbError TzifData)
loadTzifFile path = do
  loaded <- createBufferFromFile path
  case loaded of
    Left error => pure (Left (TzdbFileError (show error)))
    Right buffer => do
      bytes <- bufferBytes buffer
      pure (mapFst TzdbParseError (parseTzif bytes))
  where
    mapFst : (left -> mapped) -> Either left right -> Either mapped right
    mapFst convert (Left error) = Left (convert error)
    mapFst convert (Right value) = Right value