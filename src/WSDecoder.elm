module WSDecoder exposing (paramsResponseDecoder, resultResponseDecoder, Params, ResultResponse(..))

import Json.Decode as Decode exposing (Decoder, int, string, at, maybe, list)
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

{-type alias PlayerObj =
    { playerid : Int
    , speed : Int
    , playertype : Maybe PlayerType 
    , ptype : Maybe PType
    }-}

--variants A and B have different shape
type PlayerObj = A Int Int | B Int PlayerType PType 

playerSpdDecoder : Decoder PlayerObj
playerSpdDecoder =
    Decode.succeed A 
        |> required "playerid" int
        |> required "speed" int

playerwoSpdDecoder : Decoder PlayerObj
playerwoSpdDecoder =
    Decode.succeed B
        |> required "playerid" int
        |> required "playertype" playerTypeDecoder
        |> required "type" pTypeDecoder 

playerDecoder : Decoder PlayerObj
playerDecoder = 
    Decode.oneOf [playerSpdDecoder, playerwoSpdDecoder]
-- Params Response

type alias Params =
    { item : Item
    , player : PlayerObj
    }

paramsDecoder : Decoder Params
paramsDecoder =
    Decode.succeed Params
        |> custom (at [ "data", "item" ] itemDecoder)
        |> custom (at [ "data", "player" ] playerDecoder)

-- end "params"

-----------------------
-- "result" response --
-----------------------
-- end "result"

-- Response

type alias Response =
    { params : Params }

paramsResponseDecoder : Decoder Response
paramsResponseDecoder =
    Decode.succeed Response
        |> required "params" paramsDecoder
        --|> optional "result" resultDecoder

type ResultResponse = ResultA String | ResultB (List PlayerObj)

resultResponseDecoder : Decoder ResultResponse
resultResponseDecoder =
    Decode.oneOf [stringDecoder, listDecoder]

stringDecoder : Decoder ResultResponse
stringDecoder =
        Decode.succeed ResultA
            |> required "result" string

listDecoder : Decoder ResultResponse
listDecoder =
        Decode.succeed ResultB
            |> required "result" (list playerDecoder)

{-resultsDecoder : Decoder (List Result)
resultsDecoder =
  Decode.oneOf
    [ Decode.list resultDecoder
    , Decode.map (\result -> [result]) resultDecoder
    ]-}
