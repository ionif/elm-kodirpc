module WSDecoder exposing (responseDecoder, stringDecoder)

import Json.Decode as Decode exposing (Decoder, int, string, at, maybe)
import Json.Decode.Pipeline exposing (custom, required, optional)

-----------------------
-- "params" response --
-----------------------

-- Item

type ItemType
    = Song

type alias Item = 
    { id : Int 
    , itype : String
    }

itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "id" int
        |> required "type" string

-- Player
-- PType
type PType = Audio | Picture

parsePType : String -> Result String PType
parsePType string =
  case string of
    "audio" -> Ok Audio
    "picture" -> Ok Picture
    _ -> Err ("Invalid direction: " ++ string)

fromResult : Result String a -> Decoder a
fromResult result =
  case result of
    Ok a -> Decode.succeed a
    Err errorMessage -> Decode.fail errorMessage

pTypeDecoder : Decoder PType
pTypeDecoder =
  Decode.string |> Decode.andThen (fromResult << parsePType)

-- Player Type 
type PlayerType
    = Internal 
    | External

parsePlayerType : String -> Result String PlayerType
parsePlayerType string =
  case string of
    "internal" -> Ok Internal
    "external" -> Ok External
    _ -> Err ("Invalid direction: " ++ string)

playerTypeDecoder : Decoder PlayerType
playerTypeDecoder =
  Decode.string |> Decode.andThen (fromResult << parsePlayerType)

type alias PlayerObj =
    { playerid : Int
    , speed : Int
    , playertype : Maybe PlayerType 
    , ptype : Maybe PType
    }

playerDecoder : Decoder PlayerObj
playerDecoder =
    Decode.succeed PlayerObj
        |> required "playerid" int
        |> required "speed" int
        |> optional "playertype" (maybe playerTypeDecoder) (Just Internal)
        |> optional "type" (maybe pTypeDecoder) (Just Audio)

-- Params Response

type alias ParamsResponse =
    { item : Item
    , player : PlayerObj
    }

paramsDecoder : Decoder ParamsResponse
paramsDecoder =
    Decode.succeed ParamsResponse
        |> custom (at [ "data", "item" ] itemDecoder)
        |> custom (at [ "data", "player" ] playerDecoder)

-- end "params"

-----------------------
-- "result" response --
-----------------------
-- end "result"

-- Response

type alias Response =
    { params : ParamsResponse }

responseDecoder : Decoder Response
responseDecoder =
    Decode.succeed Response
        |> required "params" paramsDecoder
        --|> optional "result" resultDecoder

stringDecoder : Decoder String 
stringDecoder =
        Decode.field "result" Decode.string

listDecoder : Decoder (List PlayerObj)
listDecoder =
        Decode.field "result" (Decode.list playerDecoder)
{-resultsDecoder : Decoder (List Result)
resultsDecoder =
  Decode.oneOf
    [ Decode.list resultDecoder
    , Decode.map (\result -> [result]) resultDecoder
    ]-}
