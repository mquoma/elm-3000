module Main exposing (..)

import Dict exposing (..)
import Html exposing (Html, div, h1, img, table, td, text, tr)
import Html.Attributes exposing (src)
import Html.Events exposing (..)
import Random exposing (..)
import Task exposing (..)


---- MODEL ----


type alias Model =
    { grid : Dict Int (Dict Int (Maybe Int))
      , openTiles : List (Int, Int)
      , isGameOver : Bool
    }


initModel : Model
initModel =
    { grid = generateGrid --Dict.empty
      , openTiles = generateOpenTiles
      , isGameOver = False
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.batch [ 
        Random.generate PickRandomTile (Random.int 0 15)
        , Random.generate PickRandomTile (Random.int 0 14) 
        ]
    )



---- UPDATE ----


type Msg
    = NoOp
    | Roll
    | Up | Right
    | PickRandomTile Int --( Int, Int )
    | GetRandomValue (Int, Int) 
    | SetRandomValue (Int, Int) Int


generateGrid : Dict Int (Dict Int (Maybe Int))
generateGrid =
    Dict.empty
        |> Dict.insert 0
            (Dict.empty
                |> Dict.insert 0 Nothing
                |> Dict.insert 1 Nothing
                |> Dict.insert 2 Nothing
                |> Dict.insert 3 Nothing
            )
        |> Dict.insert 1
            (Dict.empty
                |> Dict.insert 0 Nothing
                |> Dict.insert 1 Nothing
                |> Dict.insert 2 Nothing
                |> Dict.insert 3 Nothing
            )
        |> Dict.insert 2
            (Dict.empty
                |> Dict.insert 0 Nothing
                |> Dict.insert 1 Nothing
                |> Dict.insert 2 Nothing
                |> Dict.insert 3 Nothing
            )
        |> Dict.insert 3
            (Dict.empty
                |> Dict.insert 0 Nothing
                |> Dict.insert 1 Nothing
                |> Dict.insert 2 Nothing
                |> Dict.insert 3 Nothing
            )

getRow x grid =
    Dict.get x grid
        |> Maybe.withDefault Dict.empty 
        |> Debug.log "row"

generateOpenTiles : List (Int, Int)
generateOpenTiles = 
    List.range 0 3
    |> List.map (\x -> 
        List.range 0 3 
            |> List.map (\y -> (x, y)))
    |> List.concat

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            --( model, (Random.generate PickRandomTile (pair (int 0 3) (int 0 3))) )
            (model, Random.generate PickRandomTile (Random.int 0 ((List.length model.openTiles - 1))))

        PickRandomTile i ->
            let
                ( x, y ) =
                    model.openTiles 
                        |> List.take (i + 1) 
                        |> List.reverse 
                        |> List.head 
                        |> Maybe.withDefault (0,0)
                        |> Debug.log "pair?"

            in
            ( model, Task.perform (always (GetRandomValue ( x, y ) )) (Task.succeed ()))

        GetRandomValue pair  ->

            ( model, Random.generate (SetRandomValue pair) (int 1 2) )

        SetRandomValue pair val ->
            let
                ( x, y ) =
                    pair 

                grid =
                    model.grid

                newOpenTiles = removeOpenTile pair model.openTiles

                row =
                    Dict.get x grid
                        |> Maybe.withDefault Dict.empty

                newRow =
                    Dict.update y (Maybe.map (\_ -> Just (val * 2))) row

                newGrid =
                    Dict.update x
                        (\_ -> Just newRow)
                        grid

                isGameOver = List.isEmpty newOpenTiles
            in
            ( { model | grid = newGrid, openTiles = newOpenTiles, isGameOver = isGameOver }, Cmd.none)


        Up ->
            ( model, Cmd.none )

        Right -> 
            let
                row = getRow 0 model.grid

                fourth = (Dict.get 3 row) |> Maybe.withDefault (Just 0)

                third = (Dict.get 2 row)  |> Maybe.withDefault (Just 0)

                newFourth = case fourth of
                    Nothing ->
                        third |> Maybe.withDefault 0
                    Just f ->
                        f + (third |> Maybe.withDefault 0)


                newRow = row 
                    |> Dict.update 3 (\_ -> Just (Just newFourth))
                    |> Dict.update 2 (\_ -> Just Nothing)

                newGrid = model.grid 
                    |> Dict.update 0 (\_ -> Just newRow)

                test = Debug.log "items" (fourth, third, newFourth)

                newOpenTiles = addOpenTile (0, 2) model.openTiles
            in
            ( {model | grid = newGrid, openTiles = newOpenTiles }, Cmd.none )

        _ ->
            ( model, Cmd.none )

removeOpenTile : (Int, Int) -> List (Int, Int) -> List (Int, Int)
removeOpenTile pair openTiles =
    openTiles
       |> List.filter(\a -> a /= pair)

addOpenTile : (Int, Int) -> List (Int, Int) -> List (Int, Int)
addOpenTile pair openTiles =
    pair :: openTiles


---- VIEW ----

renderButtons : Html Msg
renderButtons =
    div []
    [
    Html.button [
            onClick Roll
        ] [text "test"]
    , Html.button [ onClick Up ] [text "UP" ]
    , Html.button [ onClick Right ] [text "RIGHT" ]
    ]

view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is Improving!" ]
        , renderButtons
        , div [] [
        renderGrid model
            ] 
        ]


renderGrid : Model -> Html Msg
renderGrid model =
    model.grid
        |> Dict.map (\k row -> renderRow row)
        |> Dict.toList
        |> List.map (\t -> Tuple.second t)
        |> table []


renderRow : Dict Int (Maybe Int) -> Html msg
renderRow row =
    row
        |> Dict.map
            (\k a ->
                a
                    |> renderCell
                    |> text
                    |> List.singleton
                    |> td []
            )
        |> Dict.toList
        |> List.map (\t -> Tuple.second t)
        |> tr
            []


renderCell : Maybe Int -> String
renderCell a =
    case a of
        Nothing ->
            ""

        Just a ->
            a |> toString



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
