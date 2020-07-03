module WSDecoder exposing (responseDecoder, stringDecoder)

import Json.Decode as Decode exposing (Decoder, int, string, at)
import Json.Decode.Pipeline exposing (custom, required, optional)

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
    { item : Item
    , player : PlayerObj
    }

paramsDecoder : Decoder ParamsResponse
paramsDecoder =
    Decode.succeed ParamsResponse
        |> custom (at [ "data", "item" ] itemDecoder)
        |> custom (at [ "data", "player" ] playerDecoder)

responseDecoder : Decoder Response
responseDecoder =
    Decode.succeed Response
        |> required "params" paramsDecoder
        --|> optional "result" resultDecoder

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
