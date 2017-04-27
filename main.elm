module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (..)
import Time exposing (..)
import List.Extra
import Random.Extra
import Random


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
    ( Model 0 False gameTimeInit gameInit, Cmd.none )


gameInit : Game
gameInit =
    Game (List.map moleInit <| List.range 1 9)


moleInit : Int -> Mole
moleInit id =
    (Mole False 3 id)


gameTimeInit : Int
gameTimeInit =
    20


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 500
            CreateMole
        , Time.every
            1000
            Tick
        ]


type alias Model =
    { score : Int
    , started : Bool
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
    | CreateMole Time
    | PopMole (Maybe Mole)
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Whack mole ->
            let
                newModel =
                    if model.started then
                        { model | score = model.score + 1 }
                    else
                        model
            in
                ( updateMole mole newModel, Cmd.none )

        Tick _ ->
            ( tickTimer model, Cmd.none )

        Start ->
            ( { model | started = True, timer = gameTimeInit, score = 0 }, Cmd.none )

        End ->
            ( { model | started = False }, Cmd.none )

        CreateMole time ->
            if model.started then
                ( model, popUpMole model )
            else
                ( model, Cmd.none )

        PopMole (Just mole) ->
            ( updateMole mole model, Cmd.none )

        PopMole Nothing ->
            ( model, Cmd.none )


popUpMole : Model -> Cmd Msg
popUpMole model =
    Random.generate PopMole (Random.Extra.sample model.game.moles)


tickTimer : Model -> Model
tickTimer model =
    if model.started && model.timer > 0 then
        { model | timer = model.timer - 1 }
    else if model.started then
        { model | started = False }
    else
        model


mapGame : (Game -> Game) -> Model -> Model
mapGame f model =
    { model | game = f model.game }


updateMole : Mole -> Model -> Model
updateMole mole model =
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
