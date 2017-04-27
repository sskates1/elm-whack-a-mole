module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (..)
import Time exposing (..)
import List.Extra


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
    ( Model 0 0 gameInit, Cmd.none )


gameInit : Game
gameInit =
    Game (List.map moleInit <| List.range 1 9)


moleInit : Int -> Mole
moleInit id =
    (Mole False 3 id)


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 5 CreateMoal


type alias Model =
    { score : Int
    , timer : Int
    , game : Game
    }


moleRowStyle : Attribute msg
moleRowStyle =
    style
        [ ( "display", "inline-block" )
        , ( "height", "181px" )
        , ( "width", "720px" )
        ]


moleStyle : Attribute msg
moleStyle =
    style
        [ ( "float", "left" )
        , ( "width", "240px" )
        , ( "height", "181px" )
        ]


mainView : Model -> Html Msg
mainView model =
    div []
        [ button [ onClick Start ] [ text "start" ]
        , div [] [ text ("timer: " ++ toString model.timer) ]
        , div [] [ text ("score: " ++ toString model.score) ]
        , div [ moleRowStyle ] [ moleRowView <| Array.slice 0 3 <| Array.fromList model.game.moles ]
        , div [ moleRowStyle ] [ moleRowView <| Array.slice 3 6 <| Array.fromList model.game.moles ]
        , div [ moleRowStyle ] [ moleRowView <| Array.slice 6 10 <| Array.fromList model.game.moles ]
        ]


moleRowView : Array Mole -> Html Msg
moleRowView moleArray =
    div [] <| Array.toList <| Array.map moleView moleArray


moleView : Mole -> Html Msg
moleView mole =
    if mole.poppedUp then
        div [ moleStyle ] [ img [ src "/mole2.png", onClick <| Whack mole ] [] ]
    else
        div [ moleStyle ] []


type Msg
    = Whack Mole
    | Start
    | End
    | CreateMoal Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Whack mole ->
            let
                newScore =
                    model.score + 1
            in
                ( whackMole mole model, Cmd.none )

        Start ->
            ( model, Cmd.none )

        End ->
            ( model, Cmd.none )

        CreateMoal time ->
            ( model, Cmd.none )


whackMole : Mole -> Model -> Model
whackMole mole model =
    let
        gameMoles =
            List.Extra.updateIf (\x -> mole.id == x.id) changeMoleState model.game.moles

        game =
            model.game
    in
        { model | game = { game | moles = gameMoles } }


changeMoleState : Mole -> Mole
changeMoleState mole =
    { mole | poppedUp = not mole.poppedUp }


type alias Game =
    { moles : List Mole }


type alias Mole =
    { poppedUp : Bool
    , lifeTimer : Int
    , id : Int
    }
