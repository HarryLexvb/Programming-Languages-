module Main exposing (main)

{-| 
Alumnos:
 - Becerra Sipiran, Cledy Elizabeth
 - Camero Gonzalez, Ian Gabriel
 - Oviedo Sivincha, Massiel
 - Ramos Villena, Sergio Leandro
 - Villanueva Borda, Harold Alejandro
 
JUEGO 1 - COLGADO

Colgado se trata de adivinar las letras correctas para revelar una palabra completa.
Cada suposición incorrecta agrega una parte del colgado.
10 conjeturas incorrectas y el juego termina.
-}

import Browser
import Browser.Events
import Html exposing (Html, div, pre, text, br, button, node)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, rel, href)
import Html.Events exposing (onClick)
import Random
import Json.Decode as Decode
import Debug exposing (log)


type KeyType
    = Alpha Char
    | Ignored


{-| Mensajes del juego.
-}
type Msg
    = GuessedChar Char
    | Restart
    | NewRandomNumber Int
    | NextWord
    | KeyPressed KeyType


{-| Todos los estados posibles del juego.
-}
type GameStatus
    = Playing
    | Won
    | Lost
    | NotStarted


{-| La fuente de verdad del jeugo es el Model
-}
type alias Model =
    { stage : List Char
    , step : Int
    , gameStatus : GameStatus
    , guessWords : List String
    , word : String
    , wordProgress : String
    }


{-| Decoder para la tecla presionada
-}
keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toAlphaKey (Decode.field "key" Decode.string)


{-| Clasifica el mensaje del KeyPressed. Ignoraremos las letras fuera del alfabeto.
-}
toAlphaKey : String -> Msg
toAlphaKey string =
    case String.uncons string of
        Just ( char, _ ) ->
            if String.contains (String.fromChar char) alphabet then
                KeyPressed (Alpha char)
            else
                KeyPressed Ignored

        _ ->
            KeyPressed Ignored


{-| Aplicar máscara de caracteres de 'template' a la etapa actual. 
La selección de caracteres a tomar de la plantilla se define en 'sequence' 
como un solo dígito. El parámetro 'stage' le dice a la función de qué parte tomar la plantilla'.
-}
placePart : Int -> List Char -> List Char -> List Char -> List Char
placePart step template_ sequence_ stage_ =
    let
        match =
            \x y -> String.fromInt x == String.fromChar y
    in
        List.map3
            (\x y z ->
                if match step y then
                    x
                else
                    z
            )
            template_
            sequence_
            stage_


{-| Proporciona la lista de caracteres de espacio que definen un escenario vacío. 
La longitud de la Lista se define por el ancho y la altura del escenario.
-}
emptyStage : Int -> Int -> List Char
emptyStage w h =
    String.repeat (w * h) " " |> String.toList


{-| Inserte el carácter proporcionado en la Lista después de cada número X de caracteres.

    Ejemplo:
        insertEvery 3 '.' ['y','y','y','y','y','y','y']

    ...returns:
        ['y','y','y','.','y','y','y','.','y']

-}
insertEvery : Int -> Char -> List Char -> List Char
insertEvery every char oldList =
    insertEvery_ every char oldList [] 1


{-| No se llama directamente. Se llama en 'insertEvery', lo que simplifica la llamada.
-}
insertEvery_ : Int -> Char -> List Char -> List Char -> Int -> List Char
insertEvery_ every char oldList newList count =
    case oldList of
        a :: b ->
            if modBy every count == 0 then
                insertEvery_ every char b (newList ++ [ a, char ]) (count + 1)
            else
                insertEvery_ every char b (newList ++ [ a ]) (count + 1)

        [] ->
            newList


{-| Punto de entrada al juego. 
No podemos usar 'Browser.sandbox' porque nuestro juego depende del paquete 'Random' 
que utiliza '(Model, Cmd Msg)' como retorno escriba desde el 'update'.
-}
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


{-| Inicializar el estado del juego como 'NotStarted'
-}
init : () -> ( Model, Cmd Msg )
init _ =
    ( { stage = template
      , step = -1
      , gameStatus = NotStarted
      , word = ""
      , guessWords = wordList
      , wordProgress = ""
      }
    , Cmd.none
    )


{-| Responder todos los mensajes del juego y actualizar el estado del Model.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GuessedChar char ->
            evalGuess char model

        Restart ->
            ( { model
                | gameStatus = Playing
                , step = -1
                , stage = emptyStage stageWidth stageHeight
              }
            , Random.generate NewRandomNumber <|
                Random.int 0 <|
                    (List.length model.guessWords)
                        - 1
            )

        NewRandomNumber number ->
            let
                ( guessWord, updatedGuessWords ) =
                    choose number model.guessWords
            in
                ( { model
                    | word = Maybe.withDefault "" guessWord
                    , guessWords = updatedGuessWords
                    , wordProgress = maskWord '-' <| Maybe.withDefault "" guessWord
                  }
                , Cmd.none
                )

        NextWord ->
            ( { model
                | gameStatus = Playing
                , step = -1
                , stage = emptyStage stageWidth stageHeight
                , guessWords =
                    if model.guessWords == [] then
                        wordList
                    else
                        model.guessWords
              }
            , Random.generate NewRandomNumber <|
                Random.int 0 <|
                    (List.length model.guessWords)
                        - 1
            )

        KeyPressed keyType ->
            if model.gameStatus /= Playing then
                ( model, Cmd.none )
            else
                case keyType of
                    Alpha char ->
                        evalGuess (Char.toUpper char) model

                    Ignored ->
                        ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyPress keyDecoder


{-| Evaluar conjetura del carácter del jugador. Esta función se llama
desde 'update', después de presionar el botón UI o la tecla en el teclado.
-}
evalGuess : Char -> Model -> ( Model, Cmd Msg )
evalGuess char model =
    let
        trimCues =
            \x -> String.dropLeft 1 x |> String.dropRight 1

        nextStep =
            model.step + 1

        correctGuess =
            String.contains (String.fromChar char) <| trimCues model.word

        alreadyGuessed =
            String.contains (String.fromChar char) <| trimCues model.wordProgress

        nextWordProgress =
            revealLetter char model.word model.wordProgress
    in
        if correctGuess && not alreadyGuessed then
            ( { model
                | wordProgress = nextWordProgress
                , gameStatus =
                    if model.word == nextWordProgress then
                        Won
                    else
                        Playing
              }
            , Cmd.none
            )
        else
            ( { model
                | step = nextStep
                , stage = placePart nextStep template sequence model.stage
                , gameStatus =
                    if nextStep == lastSequence then
                        Lost
                    else
                        Playing
              }
            , Cmd.none
            )


{-| Elija un miembro de la Lista por índice y devuelva el miembro y una nueva lista sin el miembro.

    Ejemplo:
        choose 1 ['a', 'b', 'c']

    ...returns:
        ('b', ['a', 'c'])

-}
choose : Int -> List a -> ( Maybe a, List a )
choose index items =
    let
        item =
            List.head <| List.drop (index) items

        start =
            List.take index items

        end =
            List.drop (index + 1) items
    in
        ( item, start ++ end )


{-| Revelar las letras en la palabra enmascarada.

    Ejemplo:
        revealLetter 'o' "automobile" "a********e"

    ...returns:
        "a**o*o***e"

-}
revealLetter : Char -> String -> String -> String
revealLetter char word maskedWord =
    List.map2
        (\x y ->
            if x == char then
                x
            else
                y
        )
        (String.toList word)
        (String.toList maskedWord)
        |> String.fromList


{-| Tome una palabra y enmascárela con el carácter provisto, revelando solo
primera y última letra.

    Ejemplo:
        maskWord '*' "telephone"

    ...returns:
        "t*******e"

-}
maskWord : Char -> String -> String
maskWord maskChar word =
    let
        mask =
            String.repeat ((String.length word) - 2) <| String.fromChar maskChar
    in
        String.left 1 word ++ mask ++ String.right 1 word


{-| Toda la presentación visual se hace aquí.
Todo el estilo principal aquí se realiza a través de otras funciones como 'styleGame' y 'styleStage',
por lo que no contaminamos la función de vista principal con un montón de estilos de elementos.
-}
view : Model -> Html Msg
view model =
    div
        ([] ++ styleGame)
        [ div [ style "font-size" "2em" ] [ text "AHORCADO" ]
        , br [] []
        , pre
            ([] ++ styleStage)
            [ text <|
                String.fromList <|
                    insertEvery 11 '\n' <|
                        model.stage
            ]
        , br [] []
        , case model.gameStatus of
            Playing ->
                viewGame model

            Won ->
                viewResult "CORRECTO"

            Lost ->
                viewResult <| "INCORRECTO. La respuesta correcta es: " ++ model.word

            NotStarted ->
                viewStart
        ]


viewResult : String -> Html Msg
viewResult message =
    div []
        [ text <| message
        , br [] []
        , br [] []
        , button [ onClick NextWord ] [ text "SIGUIENTE" ]
        ]


viewStart : Html Msg
viewStart =
    div []
        [ text "TEMA: ANIMALES"
        , br [] []
        , br [] []
        , button [ onClick Restart ] [ text "INICIAR" ]
        ]


viewGame : Model -> Html Msg
viewGame model =
    div []
        [ div [ style "font-size" "1.5em" ] [ text model.wordProgress ]
        , br [] []
        , viewKeyboard
        ]


viewKeyboard : Html Msg
viewKeyboard =
    let
        alphabet_ =
            String.toList "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ"
    in
        div [] <|
            List.map
                (\x ->
                    button ([ onClick (GuessedChar x) ] ++ styleKeyboadButton)
                        [ text <| String.fromChar x ]
                )
                alphabet_


styleKeyboadButton : List (Html.Attribute Msg)
styleKeyboadButton =
    [ style "border" "1px solid gray"
    , style "background-color" "#eeeeee"
    , style "border-radius" "5px"
    , style "margin" "2px"
    , style "font-family" "inherit"
    ]


styleGame : List (Html.Attribute Msg)
styleGame =
    [ style "border" "1px solid black"
    , style "padding" "20px"
    , style "width" "200px"
    , style "margin" "0 auto"
    , style "text-align" "center"
    , style "font-family" "monospace"
    , style "background-color" "#dddddd"
    , style "zoom" "200%"
    ]


styleStage : List (Html.Attribute Msg)
styleStage =
    [ style "border-bottom" "1px solid black"
    , style "background-color" "#eeeeee"
    , style "margin" "10px"
    , style "width" "150px"
    , style "margin" "0 auto"
    , style "font-family" "monospace, monospace"
    , style "font-size" "1em"
    ]


{-| Esta plantilla funciona correctamente solo en navegadores de escritorio.
Los navegadores móviles no parecen manejar bien estos caracteres (problema con ancho fijo)
-}
xtemplate : List Char
xtemplate =
    "   ╔═════╕ "
        ++ "   ║     │ "
        ++ "   ║     O "
        ++ "   ║    /|\\"
        ++ "   ║    / \\"
        ++ "╔══╩══╗    "
        |> String.toList


{-| Plantilla para los gráficos ascii de la escena del ahorcado.
Muestra los caracteres correctamente en todos los navegadores.
-}
template : List Char
template =
    "   _______ "
        ++ "   |     | "
        ++ "   |     O "
        ++ "   |    /|\\"
        ++ "___|___ / \\"
        ++ "|     |    "
        |> String.toList


{-| Secuencia en la que se muestran las partes de la plantilla
-}
sequence : List Char
sequence =
    "   2222223 "
        ++ "   1     3 "
        ++ "   1     4 "
        ++ "   1    657"
        ++ "0001000 8 9"
        ++ "0000000    "
        |> String.toList


lastSequence : Int
lastSequence =
    9


stageWidth : Int
stageWidth =
    11


stageHeight : Int
stageHeight =
    6


{-| Todos los caracteres de adivinación permitidos desde la entrada del teclado.
Todo lo demás será filtrado.
-}
alphabet : String
alphabet =
    "abcdefghijklmnñopqrstuvwxyzABCDEFGHIJKLMNÑOPQRSTUVWXYZ"


{-| Lista de palabras del juego. Reemplace a su propio gusto.
-}
wordList : List String
wordList =
    List.map
        (\x -> String.toUpper x)
        [ "jirafa"
        , "cebra"
        , "koala"
        , "lagarto"
        , "mariposa"
        , "araña"
        , "oruga"
        , "castor"
        , "cocodrilo"
        , "loro"
        , "elefante"
        , "mono"
        , "caballo"
        , "puercoespin"
        , "caiman"
        , "tortuga"
        , "tiburon"
        , "ballena"
        , "delfin"
        , "tigre"
        ]
