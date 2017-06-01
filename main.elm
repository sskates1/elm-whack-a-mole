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
    (Mole Unpopped id)


defaultTTL : Int
defaultTTL =
    1


gameTimeInit : Int
gameTimeInit =
    20


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.started then
        Sub.batch
            [ Time.every
                500
                CreateMole
            , Time.every
                1000
                Tick
            ]
    else
        Sub.none


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
    if isPopped mole then
        div [ moleStyle ] [ img [ src "mole2.png", onClick <| Whack mole ] [] ]
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
            ( (updateMole mole << incrementScore) model, Cmd.none )

        Tick _ ->
            ( tickTimer model, Cmd.none )

        Start ->
            ( { model | started = True, timer = gameTimeInit, score = 0, game = gameInit }, Cmd.none )

        End ->
            ( { model | started = False, game = gameInit }, Cmd.none )

        CreateMole _ ->
            if model.started then
                ( model, popUpMole model )
            else
                ( model, Cmd.none )

        PopMole (Just mole) ->
            ( updateMole mole model, Cmd.none )

        PopMole Nothing ->
            ( model, Cmd.none )


incrementScore : Model -> Model
incrementScore model =
    { model | score = model.score + 1 }


popUpMole : Model -> Cmd Msg
popUpMole model =
    Random.generate PopMole (Random.Extra.sample <| List.filter (not << isPopped) model.game.moles)


tickTimer : Model -> Model
tickTimer model =
    if model.started && model.timer > 0 then
        { model | timer = model.timer - 1, game = countDownPoppedMoles model.game }
    else if model.started then
        { model | started = False, game = gameInit }
    else
        model


countDownPoppedMoles : Game -> Game
countDownPoppedMoles game =
    { game | moles = List.map reduceLifeOfMole game.moles }


reduceLifeOfMole : Mole -> Mole
reduceLifeOfMole mole =
    case mole.state of
        Popped 0 ->
            { mole | state = Unpopped }

        Popped timeToLive ->
            { mole | state = Popped (timeToLive - 1) }

        Unpopped ->
            mole


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
    case mole.state of
        Unpopped ->
            { mole | state = Popped defaultTTL }

        Popped _ ->
            { mole | state = Unpopped }


type alias Game =
    { moles : List Mole }


type MoleState
    = Popped Int
    | Unpopped


type alias Mole =
    { state : MoleState
    , id : Int
    }


isPopped : Mole -> Bool
isPopped mole =
    case mole.state of
        Popped _ ->
            True

        Unpopped ->
            False



-- mapMoleState : (Int -> Int) -> MoleState -> MoleState
-- mapMoleState func moleState =
--     case moleState of
--         Popped ttl ->
--             Popped (func ttl)
--
--         Unpopped ->
--             Unpopped
--
--
-- decrementMoleState : MoleState -> MoleState
-- decrementMoleState =
--     mapMoleState ((-) 1)
