module Main exposing (main)

-- Snake Game

import Browser
import Browser.Events
import Dict
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Set
import Svg
import Svg.Attributes
import Time


-- Tamaño del mundo
worldSizeX : Int
worldSizeX =
    16


worldSizeY : Int
worldSizeY =
    12


-- Mensajes del juego
type Msg
    = KeyDown Int
    | Tick


-- Dirección de la serpiente
type SnakeDirection
    = Up
    | Right
    | Down
    | Left


-- Estado del juego
type alias GameState =
    { snake : Snake
    , edibleLocation : Location
    }


-- Posición en el mundo
type alias Location =
    { x : Int, y : Int }


-- Serpiente
type alias Snake =
    { direction : SnakeDirection
    , segments : List Location
    }


-- Estado inicial del juego
initialState : GameState
initialState =
    { snake = { direction = Right, segments = [ { x = 4, y = 5 }, { x = 3, y = 5 } ] }
    , edibleLocation = { x = 3, y = 2 }
    }


-- Diccionario de dirección de la serpiente a partir del código de tecla
snakeDirectionFromKeyCode : Dict.Dict Int SnakeDirection
snakeDirectionFromKeyCode =
    [ ( 87, Up )  -- W
    , ( 38, Up ) -- Flecha arriba
    , ( 83, Down ) -- S
    , ( 40, Down ) -- Flecha abajo
    , ( 65, Left ) -- A
    , ( 37, Left ) -- Flecha izquierda
    , ( 68, Right ) -- D
    , ( 39, Right ) -- Flecha derecha
    ]
        |> Dict.fromList


-- Actualiza el estado del juego según el mensaje recibido
update : Msg -> GameState -> GameState
update msg gameStateBefore =
    case msg of
        KeyDown keyCode ->
            case snakeDirectionFromKeyCode |> Dict.get keyCode of
                Nothing ->
                    gameStateBefore

                Just snakeDirection ->
                    let
                        snakeBefore =
                            gameStateBefore.snake

                        snake =
                            { snakeBefore | direction = snakeDirection }
                    in
                    { gameStateBefore | snake = snake }

        Tick ->
            gameStateBefore |> advanceSnakeOneStep


-- Obtiene el desplazamiento de un paso de la serpiente según la dirección
snakeStepOffsetFromDirection : SnakeDirection -> Location
snakeStepOffsetFromDirection direction =
    case direction of
        Up ->
            { x = 0, y = -1 }

        Down ->
            { x = 0, y = 1 }

        Left ->
            { x = -1, y = 0 }

        Right ->
            { x = 1, y = 0 }


-- Avanza la serpiente un paso en el juego
advanceSnakeOneStep : GameState -> GameState
advanceSnakeOneStep gameStateBefore =
    let
        snakeBefore =
            gameStateBefore.snake
    in
    case snakeBefore.segments of
        headLocationBefore :: tailBefore ->
            let
                headMovement =
                    snakeStepOffsetFromDirection snakeBefore.direction

                headLocationBeforeWrapping =
                    { x = headLocationBefore.x + headMovement.x
                    , y = headLocationBefore.y + headMovement.y
                    }

                headLocation =
                    { x = (headLocationBeforeWrapping.x + worldSizeX) |> modBy worldSizeX
                    , y = (headLocationBeforeWrapping.y + worldSizeY) |> modBy worldSizeY
                    }

                snakeDidEat =
                    headLocation == gameStateBefore.edibleLocation

                tailBeforeEating =
                    if 0 < (tailBefore |> List.length) then
                        headLocationBefore :: (tailBefore |> List.reverse |> List.drop 1 |> List.reverse)

                    else
                        []

                tailAdditionFromEating =
                    if snakeDidEat then
                        [ tailBefore |> List.reverse |> List.head |> Maybe.withDefault headLocationBefore ]

                    else
                        []

                snakeTail =
                    tailBeforeEating ++ tailAdditionFromEating

                snakeSegments =
                    [ headLocation ] ++ snakeTail

                edibleLocation =
                    if not snakeDidEat then
                        gameStateBefore.edibleLocation

                    else
                        let
                            locationsWithoutSnake =
                                List.range 0 (worldSizeX - 1)
                                    |> List.concatMap
                                        (\x ->
                                            List.range 0 (worldSizeY - 1)
                                                |> List.map (\y -> { x = x, y = y })
                                        )
                                    |> List.filter (\candidate -> snakeSegments |> List.member candidate |> not)

                            -- TODO: Simplify to listRemoveSet
                        in
                        locationsWithoutSnake
                            |> List.drop (15485863 |> modBy ((locationsWithoutSnake |> List.length) - 1))
                            |> List.head
                            |> Maybe.withDefault { x = -1, y = -1 }
            in
            { gameStateBefore
                | snake = { snakeBefore | segments = snakeSegments }
                , edibleLocation = edibleLocation
            }

        _ ->
            gameStateBefore


-- Vista del juego
view : GameState -> Html.Html Msg
view gameState =
    let
        cellSideLength =
            40

        svgRectAtCellLocation fill location =
            Svg.rect
                [ Svg.Attributes.fill fill
                , Svg.Attributes.x (location.x * cellSideLength + 1 |> String.fromInt)
                , Svg.Attributes.y (location.y * cellSideLength + 1 |> String.fromInt)
                , Svg.Attributes.width ((cellSideLength - 2) |> String.fromInt)
                , Svg.Attributes.height ((cellSideLength - 2) |> String.fromInt)
                ]
                []

        snakeView =
            gameState.snake.segments
                |> List.map (svgRectAtCellLocation "whitesmoke")
                |> Svg.g []

        edibleView =
            svgRectAtCellLocation "red" gameState.edibleLocation
    in
    [ Svg.svg
        [ Svg.Attributes.width (worldSizeX * cellSideLength |> String.fromInt)
        , Svg.Attributes.height (worldSizeY * cellSideLength |> String.fromInt)
        , Html.Attributes.style "background" "black"
        ]
        [ snakeView, edibleView ]
    ]
        |> Html.div
            [ Html.Attributes.style "background" "grey"
            , Html.Attributes.style "width" "100vw"
            , Html.Attributes.style "height" "100vh"
            ]


-- Suscripciones del juego
subscriptions : GameState -> Sub Msg
subscriptions model =
    [ Browser.Events.onKeyDown (Html.Events.keyCode |> Json.Decode.map KeyDown)
    , Time.every 125 (always Tick)
    ]
        |> Sub.batch


-- Programa principal
main : Program () GameState Msg
main =
    Browser.element
        { init = always ( initialState, Cmd.none )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
