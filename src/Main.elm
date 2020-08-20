port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import WSDecoder exposing (ArtistObj, SongObj, ItemDetails, ParamsResponse, Item, PlayerObj(..), PType(..), paramsResponseDecoder, resultResponseDecoder, ResultResponse(..))
import Request exposing (Params, Property(..), propertyToStr, paramsToObj, request)
import Method exposing (Method(..), methodToStr, strToMethod)

-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- PORTS

port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg

-- MODEL

type alias Model =
  { draft : String
  , messages : List String
  , players : List PlayerObj
  , currentlyPlaying : ItemDetails
  }

init : () -> ( Model, Cmd Msg )
init flags =
  ( { draft = "", messages = [], players = [], currentlyPlaying = ItemDetails "" 0 ""}
  , Cmd.none
  )

-- UPDATE

type Msg
  = DraftChanged String
  | Send
  | Request Method (Maybe Params)
  | PlayPause
  | Skip
  | Recv String
  | ReceiveParamsResponse ParamsResponse
  | ReceiveResultResponse ResultResponse


-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DraftChanged draft ->
      ( { model | draft = draft }
      , Cmd.none
      )

    Send ->
      ( { model | draft = "" }
      , sendMessage model.draft
      )

    Request method params ->
      case params of
          Nothing ->
              ( model
              , sendMessage (request method Nothing)
              )
          Just param ->
              ( model
              , sendMessage (request method (Just {playerid = param.playerid, songid = Nothing, properties = param.properties}))
              )

    PlayPause ->
      ( { model | draft = "" }
      , sendMessage """{ "jsonrpc": "2.0", "method": "Input.ExecuteAction", "params": { "action": "playpause" }, "id": 1 }"""
      )

    Skip ->
      ( { model | draft = "" }
      , sendMessage """{ "jsonrpc": "2.0", "method": "Input.ExecuteAction", "params": { "action": "skipnext" }, "id": 1 }"""
      )

    Recv message ->
      ( { model | messages = model.messages ++ [message] }
      , Cmd.none
      )

    ReceiveParamsResponse params ->
      ( { model | draft = "" }
        --{"jsonrpc":"2.0","method":"AudioLibrary.GetSongDetails","params":{"songid":1, "properties":["file"]},"id":1}
        --{"jsonrpc": "2.0", "method": "Player.GetItem, params": { "properties": ["label", "duration", "id", "thumbnail"], "playerid": 0 }, "id": "AudioGetItem"}
      , sendMessage """{"jsonrpc": "2.0", "method": "Player.GetItem", "params": { "properties": ["title", "duration", "thumbnail"], "playerid": 0 }, "id": "AudioGetItem"}"""
      )

    ReceiveResultResponse result ->
      case result of 
        ResultA str ->  
          ( { model | messages = model.messages ++ [str] }
          , Cmd.none
          )
        ResultB playerObjects ->  
          ( { model | players = playerObjects }
            --chain messages, once we get players we gotta see what's playing
            --{"jsonrpc": "2.0", "method": "Player.GetItem", "params": { "properties": ["title", "album", "artist", "duration", "thumbnail", "file", "fanart", "streamdetails"], "playerid": 0 }, "id": "AudioGetItem"}
          , sendMessage 
              (request Player_GetItem 
                  ( Just 
                    { playerid = (Just 0)
                    , songid = Nothing
                    , properties = (Just [Title, Album, Artist, Duration, Thumbnail])
                    }
                  )
              )
          )
        ResultC item ->
          ( { model | currentlyPlaying = item}
          , Cmd.none)
        ResultD songlist ->
          ( { model | messages = model.messages ++ List.concatMap (\song -> song.genre) songlist }
          , Cmd.none
          )
        ResultE artistlist ->
          ( { model | messages = model.messages ++ List.concatMap (\item -> item.genre) artistlist }
          , Cmd.none
          )
        ResultF albumlist ->
          ( { model | messages = model.messages ++ List.map (\item -> item.thumbnail) albumlist }
          , Cmd.none
          )
        ResultG movielist ->
          ( { model | messages = model.messages ++ List.map (\item -> item.label) movielist }
          , Cmd.none
          )
        ResultH percent ->
          ( { model | messages = model.messages ++ [String.fromFloat(percent)] }
          , Cmd.none
          )
        ResultI sourcelist ->
          ( { model | messages = model.messages ++ List.map (\item -> item.file) sourcelist }
          , Cmd.none
          )
        ResultJ muted volume ->
          case muted of
            False ->
              ( { model | messages = model.messages ++ [ (String.fromFloat volume) ++ ", " ++ "False" ] }
              , Cmd.none
              )
            True -> 
              ( { model | messages = model.messages ++ [ (String.fromFloat volume) ++ ", " ++ "True" ] }
              , Cmd.none
              )
        ResultK filelist ->
          ( { model | messages = model.messages ++ List.map (\item -> item.label) filelist }
          , Cmd.none
          )


songname : SongObj -> String
songname song =
  song.label


-- SUBSCRIPTIONS
decodeWS message = 
    case D.decodeString paramsResponseDecoder message of 
      Ok paramsMessage ->
          ReceiveParamsResponse paramsMessage
      Err err ->
          case D.decodeString resultResponseDecoder message of 
            Ok resultMessage ->
              ReceiveResultResponse resultMessage
            Err err2 ->
              Recv message

-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--
subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver decodeWS



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "elm-kodirpc" ]
    , ul []
        (List.map (\msg -> li [] [ text msg ]) model.messages)
    , ul []
        (List.map 
          (\player -> 
            case player of
              PlayerA playerid speed ->
                li [] [ text ((String.fromInt playerid) ++ ", " ++ (String.fromInt speed)) ]
              PlayerB playerid playertype ptype ->
                case ptype of
                  Picture ->
                    li [] [ text ("Picture, " ++ (String.fromInt playerid)) ]
                  Audio ->
                    li [] [ text ("Audio, " ++ (String.fromInt playerid)) ]
          ) 
        model.players)
    , li [] [text ("Currently playing: " ++ model.currentlyPlaying.title ++ 
               " duration: " ++ String.fromInt(model.currentlyPlaying.duration)
              )]
    , input
        [ type_ "text"
        , placeholder "Draft"
        , onInput DraftChanged
        , on "keydown" (ifIsEnter Send)
        , value model.draft
        ]
        []
    , button [ onClick Send ] [ text "Send" ]
    , button [ onClick PlayPause ] [ text "Play/Pause" ]
    , button [ onClick Skip ] [ text "Skip" ]
    ]
    



-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
  D.field "key" D.string
    |> D.andThen (\key -> if key == "Enter" then D.succeed msg else D.fail "some other key")
