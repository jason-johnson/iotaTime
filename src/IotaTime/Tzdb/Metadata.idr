module IotaTime.Tzdb.Metadata

%default total

||| One TZDB link from an alias to its canonical zone identifier.
public export
record ZoneAlias where
  constructor MkZoneAlias
  aliasId : String
  canonicalId : String

||| Version and identifier metadata associated with a time-zone provider.
public export
record TzdbMetadata where
  constructor MkTzdbMetadata
  tzdbVersion : Maybe String
  zoneAliases : List ZoneAlias

versionLine : List String -> Maybe String
versionLine ["#", "version", value] = Just value
versionLine _ = Nothing

aliasLine : List String -> Maybe ZoneAlias
aliasLine ["L", canonical, alias] = Just (MkZoneAlias alias canonical)
aliasLine _ = Nothing

firstJust : (value -> Maybe result) -> List value -> Maybe result
firstJust convert [] = Nothing
firstJust convert (value :: rest) = case convert value of
  Just result => Just result
  Nothing => firstJust convert rest

isWhitespace : Char -> Bool
isWhitespace ' ' = True
isWhitespace '\t' = True
isWhitespace _ = False

tokens : String -> List String
tokens source = go [] [] (unpack source)
  where
    finish : List Char -> List String -> List String
    finish [] found = reverse found
    finish current found = reverse (pack (reverse current) :: found)

    go : List Char -> List String -> List Char -> List String
    go current found [] = finish current found
    go current found (value :: rest) =
      if isWhitespace value
        then case current of
          [] => go [] found rest
          _ => go [] (pack (reverse current) :: found) rest
        else go (value :: current) found rest

sourceLines : String -> List String
sourceLines source = go [] (unpack source)
  where
    go : List Char -> List Char -> List String
    go current [] = [pack (reverse current)]
    go current ('\r' :: '\n' :: rest) =
      pack (reverse current) :: go [] rest
    go current ('\n' :: rest) = pack (reverse current) :: go [] rest
    go current (value :: rest) = go (value :: current) rest

||| Parse the version declaration and Link records from a `tzdata.zi` file.
public export
parseTzdataIdentity : String -> (Maybe String, List ZoneAlias)
parseTzdataIdentity source =
  let tokenLines = map tokens (sourceLines source)
   in (firstJust versionLine tokenLines, mapMaybe aliasLine tokenLines)

findAlias : String -> List ZoneAlias -> Maybe String
findAlias value [] = Nothing
findAlias value (alias :: rest) =
  if alias.aliasId == value then Just alias.canonicalId
  else findAlias value rest

||| Resolve a TZDB link chain. Cycles terminate after one pass over the aliases.
public export
canonicalZoneId : TzdbMetadata -> String -> String
canonicalZoneId metadata value = go (length metadata.zoneAliases) value
  where
    go : Nat -> String -> String
    go Z current = current
    go (S fuel) current = case findAlias current metadata.zoneAliases of
      Nothing => current
      Just canonical => go fuel canonical
