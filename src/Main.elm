port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import WSDecoder exposing (PlayerObj(..), paramsResponseDecoder, resultResponseDecoder, Params, ResultResponse(..))

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
  }


init : () -> ( Model, Cmd Msg )
init flags =
  ( { draft = "", messages = [], players = [] }
  , Cmd.none
  )


-- UPDATE

type Msg
  = DraftChanged String
  | Send
  | PlayPause
  | Skip
  | Recv String
  | ReceiveParamsResponse String
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
      {-case message.params.player of
        A a ->
          ( { model | messages = model.messages ++ [message] } --wsMessage.params.item.itype
          , Cmd.none
          )
        B b ->-}
      ( { model | messages = model.messages ++ [params] } --wsMessage.params.item.itype
      , Cmd.none
      )

    ReceiveResultResponse result ->
      case result of 
        ResultA str ->  
          ( { model | messages = model.messages ++ [str] }
          , Cmd.none
          )
        ResultB playerObjects ->  
          ( { model | players = playerObjects }
          , Cmd.none
          )


-- SUBSCRIPTIONS
decodeWS message = 
    case D.decodeString paramsResponseDecoder message of 
      Ok wsMessage ->
          ReceiveParamsResponse message
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
    [ h1 [] [ text "Echo Chat" ]
    , ul []
        (List.map (\msg -> li [] [ text msg ]) model.messages)
    , ul []
        (List.map 
          (\player -> 
            case player of
              PlayerA playerid speed ->
                li [] [ text ((String.fromInt playerid) ++ ", " ++ (String.fromInt speed)) ]
              PlayerB playerid playertype ptype ->
                li [] [ text (String.fromInt playerid) ]
          ) 
        model.players)
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
