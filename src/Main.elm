module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (Seed)
import Set exposing (Set)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



{-
   Any live cell with fewer than two live neighbours dies, as if by underpopulation.
   Any live cell with two or three live neighbours lives on to the next generation.
   Any live cell with more than three live neighbours dies, as if by overpopulation.
   Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
-}


type Min a
    = Min a


type Max a
    = Max a


type alias Model =
    { state : State
    , seed : Seed
    , automate : Bool
    , stepInterval : Float
    }


type State
    = SelectingGrid (Set ( Int, Int ))
    | RunningSimulation (Set ( Int, Int ))


type Cell
    = Alive
    | Dead


bounds : (( Int, Int ) -> Int) -> Set ( Int, Int ) -> ( Min Int, Max Int )
bounds op d =
    d
        |> Set.toList
        |> List.map op
        |> List.foldr
            (\v ( Min min, Max max ) ->
                ( Min <|
                    if v < min then
                        v

                    else
                        min
                , Max <|
                    if v > max then
                        v

                    else
                        max
                )
            )
            ( Min 0, Max 0 )


xBounds : Set ( Int, Int ) -> ( Min Int, Max Int )
xBounds =
    bounds Tuple.first


yBounds : Set ( Int, Int ) -> ( Min Int, Max Int )
yBounds =
    bounds Tuple.second


allCoords : ( Min Int, Max Int ) -> ( Min Int, Max Int ) -> List ( Int, Int )
allCoords ( Min xMin, Max xMax ) ( Min yMin, Max yMax ) =
    List.range (xMin - 1) (xMax + 1)
        |> List.concatMap (\x -> List.range (yMin - 1) (yMax + 1) |> List.map (\y -> ( x, y )))


neighbours : ( Int, Int ) -> List ( Int, Int )
neighbours ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    ]


getCellState s cell =
    let
        isLive =
            Set.member cell s

        liveNeighbours =
            neighbours cell
                |> Set.fromList
                |> Set.intersect s

        isStillAlive =
            isLive
                && (Set.size liveNeighbours
                        == 2
                        || Set.size liveNeighbours
                        == 3
                   )
                || (not isLive && Set.size liveNeighbours == 3)
    in
    if isStillAlive then
        Just cell

    else
        Nothing


step : Set ( Int, Int ) -> Set ( Int, Int )
step s =
    let
        coords =
            allCoords (xBounds s) (yBounds s)
    in
    coords
        |> List.filterMap (getCellState s)
        |> Set.fromList


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = SelectingGrid Set.empty
      , seed = Random.initialSeed 0
      , automate = False
      , stepInterval = 1000
      }
    , Cmd.none
    )


type Msg
    = Populate
    | Step
    | SelectGridCell ( Int, Int )
    | ResetSelections
    | StartSimulation
    | Automate
    | AdjustInterval String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Populate ->
            ( model, Cmd.none )

        Step ->
            case model.state of
                RunningSimulation g ->
                    ( { model | state = RunningSimulation (step g) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectGridCell cell ->
            case model.state of
                SelectingGrid g ->
                    ( { model
                        | state =
                            SelectingGrid
                                (if Set.member cell g then
                                    Set.remove cell g

                                 else
                                    Set.insert cell g
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ResetSelections ->
            case model.state of
                SelectingGrid _ ->
                    ( { model | state = SelectingGrid Set.empty }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartSimulation ->
            case model.state of
                SelectingGrid g ->
                    ( { model | state = RunningSimulation g }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Automate ->
            ( { model | automate = not model.automate }, Cmd.none )

        AdjustInterval int ->
            ( { model
                | stepInterval =
                    String.toFloat int |> Maybe.withDefault 1000
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.automate then
        Time.every model.stepInterval (\_ -> Step)

    else
        Sub.none


fullGrid : Set ( Int, Int ) -> List ( ( Int, Int ), Cell )
fullGrid live =
    let
        coords =
            allCoords (xBounds live) (yBounds live)

        toCell x =
            case x of
                Just _ ->
                    Alive

                Nothing ->
                    Dead

        toMaybe x =
            if x then
                Just ()

            else
                Nothing
    in
    coords
        |> List.map
            (\c ->
                ( c
                , live
                    |> Set.member c
                    |> toMaybe
                    |> toCell
                )
            )


tileSize =
    20


view : Model -> Html Msg
view model =
    let
        gridTile ( ( x, y ), cellState ) =
            div
                [ style "position" "absolute"
                , style "transform" ("translate(calc(50vw + " ++ String.fromInt (tileSize * x) ++ "px), calc(50vh + " ++ String.fromInt (tileSize * y) ++ "px))")
                , style "border" "1px solid black"
                , style "height" (String.fromInt tileSize ++ "px")
                , style "width" (String.fromInt tileSize ++ "px")
                , style "transition" "all ease-in-out 200ms"
                , Html.Attributes.attribute "data-cell" ("(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")")
                , style "background-color"
                    (case cellState of
                        Alive ->
                            "black"

                        Dead ->
                            "white"
                    )
                ]
                []
    in
    case model.state of
        RunningSimulation g ->
            div
                [ style "height" "100vh"
                , style "width" "100vw"
                , style "display" "flex"
                , style "flex-direction" "column"
                ]
                [ if model.automate then
                    div []
                        [ button [ onClick Automate ]
                            [ text
                                (if model.automate then
                                    "Stop Automation"

                                 else
                                    "Automate"
                                )
                            ]
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "500"
                            , Html.Attributes.max "10000"
                            , Html.Attributes.step "500"
                            , value (String.fromFloat model.stepInterval)
                            , onInput AdjustInterval
                            ]
                            []
                        , text (String.fromFloat (model.stepInterval / 1000) ++ "s")
                        ]

                  else
                    div []
                        [ button [ onClick Automate ]
                            [ text
                                (if model.automate then
                                    "Stop Automation"

                                 else
                                    "Automate"
                                )
                            ]
                        , button [ onClick Step ]
                            [ text "Step" ]
                        ]
                , div
                    [ style "position" "relative"
                    , style "flex-grow" "1"
                    ]
                    (List.map gridTile (fullGrid g))
                ]

        SelectingGrid g ->
            div
                [ style "height" "100vh"
                , style "width" "100vw"
                , style "display" "flex"
                , style "flex-direction" "column"
                ]
                [ div []
                    [ button [ onClick ResetSelections ] [ text "Reset" ]
                    , button [ onClick StartSimulation ] [ text "Start the simulation" ]
                    ]
                , div
                    [ style "position" "relative"
                    , style "flex-grow" "1"
                    ]
                    [ selectingView g ]
                ]


selectingView : Set ( Int, Int ) -> Html Msg
selectingView g =
    let
        newTileSize =
            10
    in
    List.range -30 30
        |> List.concatMap (\x -> List.range -30 30 |> List.map (\y -> ( x, y )))
        |> List.map
            (\( x, y ) ->
                div
                    [ style "position" "absolute"
                    , style "transform" ("translate(calc(50vw + " ++ String.fromInt (newTileSize * x) ++ "px), calc(50vh + " ++ String.fromInt (newTileSize * y) ++ "px))")
                    , style "border" "1px solid black"
                    , style "height" (String.fromInt newTileSize ++ "px")
                    , style "width" (String.fromInt newTileSize ++ "px")
                    , style "transition" "all ease-in-out 300ms"
                    , onClick (SelectGridCell ( x, y ))
                    , Html.Attributes.attribute "data-cell" ("(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")")
                    , style "background-color"
                        (if Set.member ( x, y ) g then
                            "black"

                         else
                            "white"
                        )
                    ]
                    []
            )
        |> div []
