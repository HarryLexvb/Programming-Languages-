module Main exposing (GameBoard, Model, Msg, Player, Row, main)

import Browser
import Element exposing (Attribute, Element, column, fill, fillPortion, height, layout, padding, paragraph, spaceEvenly, text, width, wrappedRow)
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import List exposing (all, any, filterMap, foldr, indexedMap, length, map2, range, repeat)
import List.Extra exposing (getAt, reverseRange, setAt, updateAt)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type Player
    = X
    | O


type alias Row =
    List (Maybe Player)


type alias Diagonal =
    Row


type alias GameBoard =
    List Row


type alias Model =
    { board : GameBoard, currentPlayer : Player, boardSize : Int }


init : { board : GameBoard, currentPlayer : Player, boardSize : Int }
init =
    initHelper 3


initHelper : Int -> { board : GameBoard, currentPlayer : Player, boardSize : Int }
initHelper boardSize =
    let
        board : GameBoard
        board =
            Nothing |> repeat boardSize |> repeat boardSize
    in
    { board = board, currentPlayer = X, boardSize = boardSize }



-- UPDATE


type Msg
    = Clicked Int Int
    | IncrementedSize
    | DecrementedSize



-- update after click


updateRow : Int -> Player -> Row -> Row
updateRow colIndex player row =
    setAt colIndex (Just player) row


updateBoard : Int -> Int -> Player -> GameBoard -> GameBoard
updateBoard rowIndex colIndex player board =
    updateAt rowIndex (updateRow colIndex player) board


updateBoardSafe : Int -> Int -> Player -> GameBoard -> GameBoard
updateBoardSafe rowIndex colIndex player board =
    if computeWinner board /= Nothing then
        board

    else
        updateBoard rowIndex colIndex player board


-- full : GameBoard -> Bool
-- full board =
--     all (\x -> x /= Nothing) (concat board)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Clicked rowIndex colIndex ->
            let
                newBoard : GameBoard
                newBoard =
                    updateBoardSafe rowIndex colIndex model.currentPlayer model.board
            in
            -- if full newBoard then
            --     initHelper model.boardSize

            -- else
                { model | board = newBoard, currentPlayer = nextPlayer model.currentPlayer }

        IncrementedSize ->
            initHelper (model.boardSize + 1)

        DecrementedSize ->
            initHelper (max (model.boardSize - 1) 2)


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        X ->
            O

        O ->
            X



-- winnig condition


computeWinner : GameBoard -> Maybe Player
computeWinner board =
    if won X board then
        Just X

    else if won O board then
        Just O

    else
        Nothing


won : Player -> GameBoard -> Bool
won player board =
    any (all (\x -> x == Just player)) (board ++ transpose board ++ [ firstDiagonal board, secondDiagonal board ])


transpose : GameBoard -> GameBoard
transpose board =
    foldr (map2 (::)) (repeat (length board - 1) []) board


diagonalHelper : List Int -> GameBoard -> Diagonal
diagonalHelper indices board =
    map2 getAt indices board |> filterMap (\x -> x)


firstDiagonal : GameBoard -> Diagonal
firstDiagonal gameBoard =
    diagonalHelper (range 0 (length gameBoard - 1)) gameBoard


secondDiagonal : GameBoard -> Diagonal
secondDiagonal gameBoard =
    diagonalHelper (reverseRange (length gameBoard) 0) gameBoard



-- view


boxStyle : List (Attribute msg)
boxStyle =
    [ height fill
    , width fill
    , Border.width 3
    , Font.size 50
    , Font.center
    ]


showBox : Int -> Int -> Maybe Player -> Element Msg
showBox rowIndex colIndex maybePlayer =
    case maybePlayer of
        Nothing ->
            button boxStyle { onPress = Just (Clicked rowIndex colIndex), label = text " " }

        Just O ->
            button boxStyle { onPress = Nothing, label = text "O" }

        Just X ->
            button boxStyle { onPress = Nothing, label = text "X" }


rowToElement : Int -> Row -> Element Msg
rowToElement rowIndex row =
    wrappedRow [ height fill, width fill ] (indexedMap (showBox rowIndex) row)


boardToElement : GameBoard -> Element Msg
boardToElement board =
    column [ height fill, width (fillPortion 9) ] (indexedMap rowToElement board)


viewHelper : { a | board : GameBoard } -> Element Msg
viewHelper model =
    wrappedRow [ height fill, width fill ] [ boardToElement model.board, secondColumn model.board ]


playerToString : Maybe Player -> String
playerToString maybePlayer =
    case maybePlayer of
        Just O ->
            "O"

        Just X ->
            "X"

        Nothing ->
            "_________"


displayWinner : GameBoard -> Element msg
displayWinner board =
    paragraph [ width fill, spaceEvenly, Font.center, height (fillPortion 2), Font.size 50 ] [ text (String.concat [ "The winner is: ", playerToString (computeWinner board) ]) ]


secondColumn : GameBoard -> Element Msg
secondColumn board =
    column [ width (fillPortion 7), spaceEvenly, Font.center, height fill, padding 50 ] [ displayWinner board, incrementButton, decremenctButton ]


incrementButton : Element Msg
incrementButton =
    button [ height (fillPortion 2), Font.center, width fill, Font.size 50 ] { onPress = Just IncrementedSize, label = text "+" }


decremenctButton : Element Msg
decremenctButton =
    button [ height (fillPortion 2), Font.center, width fill, Font.size 50 ] { onPress = Just DecrementedSize, label = text "-" }


view : { a | board : GameBoard } -> Html Msg
view model =
    layout [] (viewHelper model)

