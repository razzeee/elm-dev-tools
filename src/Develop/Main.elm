module Develop.Main exposing (toInit, toMsg, toSubscriptions, toUpdate, toView)

import Browser as B
import Browser.Config as Bc
import Browser.Navigation as Bn
import Html as H
import Url exposing (Url)


type Msg msg
    = UpdateMsg msg


type alias Model model msg =
    { initialModel : model
    , modelUpdates : List ( model, msg )
    }


toMsg : msg -> Msg msg
toMsg =
    UpdateMsg


toInit : Bc.Init model msg flags -> Bc.Init (Model model msg) (Msg msg) flags
toInit init flags url key =
    let
        ( model, cmd ) =
            init flags url key
    in
    ( { initialModel = model
      , modelUpdates = []
      }
    , Cmd.map UpdateMsg cmd
    )


toUpdate : Bc.Update model msg -> Bc.Update (Model model msg) (Msg msg)
toUpdate update msg model =
    case msg of
        UpdateMsg updateMsg ->
            let
                ( updateModel, updateCmd ) =
                    update updateMsg (toLatestModel model)
            in
            ( { model | modelUpdates = ( updateModel, updateMsg ) :: model.modelUpdates }
            , Cmd.map UpdateMsg updateCmd
            )


toSubscriptions : Bc.Subscriptions model msg -> Bc.Subscriptions (Model model msg) (Msg msg)
toSubscriptions subscriptions model =
    Sub.map UpdateMsg (subscriptions (toLatestModel model))


toView : Bc.View model msg -> Bc.View (Model model msg) (Msg msg)
toView view model =
    let
        { title, body } =
            view (toLatestModel model)
    in
    { title = title
    , body = List.map (H.map UpdateMsg) body
    }


toLatestModel :
    { record
        | initialModel : model
        , modelUpdates : List ( model, msg )
    }
    -> model
toLatestModel { initialModel, modelUpdates } =
    case List.head modelUpdates of
        Just ( updatedModel, _ ) ->
            updatedModel

        Nothing ->
            initialModel
