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
    }


initModel : Model
initModel =
    { grid = generateGrid --Dict.empty
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.batch [ 
        Random.generate PickRandomTile (pair (int 0 3) (int 0 3)) 
        , Random.generate PickRandomTile (pair (int 0 3) (int 0 3)) 
    ] 
    )



---- UPDATE ----


type Msg
    = NoOp
    | Roll
    | PickRandomTile ( Int, Int )
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, (Random.generate PickRandomTile (pair (int 0 3) (int 0 3))) )

        PickRandomTile pair ->
            let
                ( x, y ) =
                    pair 
            in
            ( model, Task.perform (always (GetRandomValue (pair))) (Task.succeed ()))

        GetRandomValue pair  ->

            ( model, Random.generate (SetRandomValue pair) (int 1 2) )

        SetRandomValue pair val ->
            let
                ( x, y ) =
                    pair 

                grid =
                    model.grid

                row =
                    Dict.get x grid
                        |> Maybe.withDefault Dict.empty

                newRow =
                    Dict.update y (Maybe.map (\_ -> Just (val * 2))) row

                newGrid =
                    Dict.update x
                        (\_ -> Just newRow)
                        grid
            in
            ( { model | grid = newGrid }, Cmd.none)
        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App sucks!" ]
        , Html.button [
            onClick Roll
        ] [text "go"]
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
