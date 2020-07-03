module WSDecoder exposing (responseDecoder, stringDecoder)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (custom, required)

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
        |> required "type" typeDecoder

typeDecoder : Decoder String 
typeDecoder =
        Decode.field "type" Decode.string

-- Data

type alias Data =
    { item : Item
    , player : PlayerObj }

dataDecoder : Decoder Data
dataDecoder =
    Decode.succeed Data
        |> required "item" itemDecoder
        |> required "player" playerDecoder

-- Player

type PlayerType
    = Audio 
    | Picture

type alias PlayerObj =
    { playerid : Int
    , speed : Int
    }

playerDecoder : Decoder PlayerObj
playerDecoder =
    Decode.succeed PlayerObj
        |> required "playerid" int
        |> required "speed" int

speedDecoder : Decoder Int
speedDecoder =
    Decode.field "speed" Decode.int

-- Params Response

type alias ParamsResponse =
    { data : Data
    }

paramsResponseDecoder : Decoder ParamsResponse
paramsResponseDecoder =
    Decode.succeed ParamsResponse
        |> required "data" dataDecoder --custom (at [ "data", "item" ] itemDecoder)

responseDecoder : Decoder Response
responseDecoder =
    Decode.succeed Response
        |> required "params" paramsResponseDecoder

type alias Response =
    { result : ParamsResponse }

stringDecoder : Decoder String 
stringDecoder =
        Decode.field "result" Decode.string
{-resultsDecoder : Decoder (List Result)
resultsDecoder =
  Decode.oneOf
    [ Decode.list resultDecoder
    , Decode.map (\result -> [result]) resultDecoder
    ]-}
