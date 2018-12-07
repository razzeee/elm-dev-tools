module Main exposing (encodeMsg, init, msgDecoder, onUrlChange, onUrlRequest, subscriptions, update, view)

import Browser as B
import Browser.Navigation as Bn
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Json.Decode as Jd
import Json.Encode as Je
import Url exposing (Url)


type alias Flags =
    {}


type alias Model =
    {}


type Msg
    = UrlRequest B.UrlRequest
    | UrlChange Url


view : Model -> B.Document Msg
view _ =
    { title = "New Elm App"
    , body =
        []
    }


init : Flags -> Url -> Bn.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( {}, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequest request ->
            ( model, Cmd.none )

        UrlChange url ->
            ( model, Cmd.none )


encodeMsg : Msg -> Je.Value
encodeMsg msg =
    case msg of
        UrlRequest request ->
            Je.null

        UrlChange url ->
            Je.null


msgDecoder : Jd.Decoder Msg
msgDecoder =
    Jd.oneOf
        []


onUrlChange =
    UrlChange


onUrlRequest =
    UrlRequest


main =
    B.application
        { init = init
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
