module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- import Graphics.Input exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = mainView
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( Model (0) (0) (gameInit), Cmd.none )


gameInit : Game
gameInit =
    Game (List.repeat 9 (Mole False 5))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { score : Int
    , timer : Int
    , game : Game
    }


mainView : Model -> Html Msg
mainView model =
    div []
        [ button [ onClick Start ] [ text "start" ]
        , div [] [ text ("timer: " ++ toString model.timer) ]
        , div [] [ text ("score: " ++ toString model.score) ]
        , div [] <| List.map moleView model.game.moles
        ]


moleView : Mole -> Html Msg
moleView mole =
    div [] [ img [ src "/mole2.png", onClick <| Whack mole ] [] ]


type Msg
    = Whack Mole
    | Start
    | End
    | CreateMoal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Whack mole ->
            let
                newScore =
                    model.score + 1
            in
                ( { model | score = newScore }, Cmd.none )

        Start ->
            ( model, Cmd.none )

        End ->
            ( model, Cmd.none )

        CreateMoal ->
            ( model, Cmd.none )


type alias Game =
    { moles : List Mole }


type alias Mole =
    { poppedUp : Bool
    , lifeTimer : Int
    }
