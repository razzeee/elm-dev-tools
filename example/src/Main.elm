port module Main exposing (main)

import Browser
import Browser.Dom as Bd
import Browser.Events as Be
import DevTools.Browser
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Json.Decode as Jd
import Json.Encode as Je
import Task
import Time


port output : Je.Value -> Cmd msg


type alias Velocity =
    { vertical : Float
    , horizontal : Float
    }


type alias Size =
    { width : Int
    , height : Int
    }


type alias Position =
    { left : Float
    , top : Float
    }


type Direction
    = Vertical VerticalDirection
    | Horizontal HorizontalDirection


type HorizontalDirection
    = Left
    | Right


type VerticalDirection
    = Up
    | Down


type alias Controls =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    }


type Msg
    = NextFrame Float
    | WindowResize Int Int
    | Press Direction
    | Release Direction


type alias Model =
    { face : HorizontalDirection
    , position : Position
    , velocity : Velocity
    , size : Size
    , controls : Controls
    }


{--}
main =
    DevTools.Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , devTools =
            { printModel = Debug.toString
            , output = output
            , msgDecoder = msgDecoder
            , encodeMsg = encodeMsg
            }
        }
--}



{--
main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
--}


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { face = Right
      , position = Position 0 0
      , velocity = Velocity 0 0
      , size = Size 0 0
      , controls = Controls False False False False
      }
    , Task.perform fromViewport Bd.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextFrame latency ->
            let
                adjustedLatency =
                    latency / 10
            in
            ( model
                |> gravity adjustedLatency
                |> jump
                |> walk
                |> physics adjustedLatency
            , Cmd.none
            )

        WindowResize width height ->
            ( physics 1
                { model
                    | size = Size width height
                }
            , Cmd.none
            )

        Press dir ->
            ( { model
                | controls = updateControls True dir model.controls
              }
            , Cmd.none
            )

        Release dir ->
            ( { model
                | controls = updateControls False dir model.controls
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Be.onAnimationFrameDelta NextFrame
        , Be.onResize WindowResize
        , Be.onKeyDown (Jd.map Press (Jd.andThen toDirectionDecoder (Jd.field "key" Jd.string)))
        , Be.onKeyUp (Jd.map Release (Jd.andThen toDirectionDecoder (Jd.field "key" Jd.string)))
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Example"
    , body =
        [ viewMario model
        ]
    }


viewMario : Model -> Html Msg
viewMario { face, position, velocity, size } =
    H.img
        [ Ha.style "position" "absolute"
        , Ha.style "left" (toPx position.left)
        , Ha.style "top" (toPx (toFloat size.height - 35 - position.top))
        , Ha.src (toImageSrc size position velocity face)
        ]
        []


toImageSrc : Size -> Position -> Velocity -> HorizontalDirection -> String
toImageSrc size position velocity face =
    "mario/"
        ++ toPoseString size position velocity
        ++ "-"
        ++ directionToString (Horizontal face)
        ++ ".gif"


gravity : Float -> Model -> Model
gravity adjustedLatency ({ velocity, size } as model) =
    { model
        | velocity =
            { velocity
                | vertical =
                    if model.position.top > 0 then
                        velocity.vertical - adjustedLatency / 4

                    else
                        0
            }
    }


jump : Model -> Model
jump ({ velocity, controls } as model) =
    if controls.up && velocity.vertical == 0 then
        { model | velocity = { velocity | vertical = 8 } }

    else
        model


walk : Model -> Model
walk ({ velocity, controls, face } as model) =
    { model
        | face = updateFace controls face
        , velocity = updateHorizontalVelocity controls velocity
    }


physics : Float -> Model -> Model
physics adjustedLatency ({ position, velocity, size } as model) =
    { model
        | position =
            { position
                | left = clamp 0 (toFloat size.width - 35) (position.left + adjustedLatency * velocity.horizontal)
                , top = clamp 0 (toFloat size.height - 35) (position.top + adjustedLatency * velocity.vertical)
            }
    }


updateFace : Controls -> HorizontalDirection -> HorizontalDirection
updateFace { left, right } face =
    if left then
        Left

    else if right then
        Right

    else
        face


updateHorizontalVelocity : Controls -> Velocity -> Velocity
updateHorizontalVelocity { left, right } velocity =
    { velocity
        | horizontal =
            if left then
                -1

            else if right then
                1

            else
                0
    }


updateControls : Bool -> Direction -> Controls -> Controls
updateControls isPressed direction controls =
    case direction of
        Vertical Up ->
            { controls | up = isPressed }

        Vertical Down ->
            { controls | down = isPressed }

        Horizontal Left ->
            { controls | left = isPressed }

        Horizontal Right ->
            { controls | right = isPressed }


stringAndThenDecoder : Jd.Decoder value -> Jd.Decoder value
stringAndThenDecoder valueDecoder =
    Jd.andThen
        (\str ->
            case Jd.decodeString valueDecoder str of
                Ok value ->
                    Jd.succeed value

                Err error ->
                    Jd.fail (Jd.errorToString error)
        )
        Jd.string


toDirectionDecoder : String -> Jd.Decoder Direction
toDirectionDecoder text =
    case text of
        "ArrowUp" ->
            Jd.succeed (Vertical Up)

        "ArrowDown" ->
            Jd.succeed (Vertical Down)

        "ArrowLeft" ->
            Jd.succeed (Horizontal Left)

        "ArrowRight" ->
            Jd.succeed (Horizontal Right)

        _ ->
            Jd.fail ("not a direction: " ++ text)


directionToString : Direction -> String
directionToString dir =
    case dir of
        Vertical Up ->
            "ArrowUp"

        Vertical Down ->
            "ArrowDown"

        Horizontal Left ->
            "ArrowLeft"

        Horizontal Right ->
            "ArrowRight"


toPoseString : Size -> Position -> Velocity -> String
toPoseString size { top } { horizontal } =
    if top > 0 then
        "jump"

    else if horizontal /= 0 then
        "walk"

    else
        "stand"


toPx : Float -> String
toPx n =
    String.fromFloat n ++ "px"


fromViewport : Bd.Viewport -> Msg
fromViewport { scene } =
    WindowResize (round scene.width) (round scene.height)


msgDecoder : Jd.Decoder Msg
msgDecoder =
    Jd.oneOf
        [ Jd.map NextFrame (Jd.field "Frame" Jd.float)
        , Jd.map2 WindowResize
            (Jd.at [ "Resize", "width" ] Jd.int)
            (Jd.at [ "Resize", "height" ] Jd.int)
        , Jd.map Press (Jd.field "Press" (Jd.andThen toDirectionDecoder Jd.string))
        , Jd.map Release (Jd.field "Release" (Jd.andThen toDirectionDecoder Jd.string))
        ]


encodeMsg : Msg -> Je.Value
encodeMsg msg =
    case msg of
        NextFrame latency ->
            Je.object [ ( "Frame", Je.float latency ) ]

        WindowResize width height ->
            Je.object
                [ ( "Resize"
                  , Je.object
                        [ ( "width", Je.int width )
                        , ( "height", Je.int height )
                        ]
                  )
                ]

        Press direction ->
            Je.object
                [ ( "Press"
                  , Je.string (directionToString direction)
                  )
                ]

        Release direction ->
            Je.object
                [ ( "Release"
                  , Je.string (directionToString direction)
                  )
                ]
