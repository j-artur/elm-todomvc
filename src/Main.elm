port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (autofocus, checked, class, classList, for, hidden, href, id, name, placeholder, style, type_, value)
import Html.Events exposing (onBlur, onClick, onDoubleClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Task
import Time exposing (Month(..), Posix, Weekday(..))
import Todo exposing (..)
import Util exposing (..)



-- PORTS


port setStorage : Value -> Cmd msg



-- MODEL


type Visibility
    = All
    | Active
    | Completed


type alias Model =
    { todos : List Todo
    , field : String
    , id : Int
    , visibility : Visibility
    , now : Posix
    }


emptyModel : Model
emptyModel =
    { todos = []
    , visibility = All
    , field = ""
    , id = 0
    , now = Time.millisToPosix 0
    }


init : Maybe Value -> ( Model, Cmd Msg )
init maybeModel =
    case maybeModel of
        Just model ->
            ( Decode.decodeValue modelDecoder model |> Result.withDefault emptyModel, Cmd.none )

        Nothing ->
            ( emptyModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | UpdateField String
    | EditingTodo Int Bool
    | UpdateTodo Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | ChangeVisibility Visibility
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                NoOp ->
                    model

                Add ->
                    { model
                        | id = model.id + 1
                        , field = ""
                        , todos =
                            if String.isEmpty model.field then
                                model.todos

                            else
                                newTodo model.field model.id :: model.todos
                    }

                UpdateField str ->
                    { model | field = str }

                EditingTodo id isEditing ->
                    let
                        updateTodo : Todo -> Todo
                        updateTodo todo =
                            if todo.id == id then
                                { todo | editing = isEditing }

                            else
                                todo
                    in
                    { model | todos = List.map updateTodo model.todos }

                UpdateTodo id task ->
                    let
                        updateTodo : Todo -> Todo
                        updateTodo todo =
                            if todo.id == id then
                                { todo | description = task }

                            else
                                todo
                    in
                    { model | todos = List.map updateTodo model.todos }

                Delete id ->
                    { model | todos = List.filter (\t -> t.id /= id) model.todos }

                DeleteComplete ->
                    { model | todos = List.filter (not << Todo.isCompleted) model.todos }

                Check id isCompleted ->
                    let
                        updateTodo : Todo -> Todo
                        updateTodo todo =
                            if todo.id == id then
                                { todo
                                    | completed =
                                        if isCompleted then
                                            Just model.now

                                        else
                                            Nothing
                                }

                            else
                                todo
                    in
                    { model | todos = List.map updateTodo model.todos }

                ChangeVisibility visibility ->
                    { model | visibility = visibility }

                Tick now ->
                    { model | now = now }

        cmd =
            case msg of
                EditingTodo id _ ->
                    Task.attempt (\_ -> NoOp) (Dom.focus ("todo-" ++ String.fromInt id))

                _ ->
                    Cmd.none
    in
    ( newModel, cmd )


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel, Cmd.batch [ setStorage <| encodeModel newModel, cmds ] )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "todomvc-wrapper" ]
        [ section
            [ class "todoapp" ]
            [ viewForm model.field
            , viewTodos model.now model.visibility model.todos
            , viewControls model.visibility model.todos
            ]
        , footer [ class "info" ]
            [ p [] [ text "Double-click to edit a todo" ]
            , p [] [ text "Written by ", a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ] ]
            , p [] [ text "Forked by ", a [ href "https://github.com/j-artur" ] [ text "João Artur" ] ]
            , p [] [ text "Part of ", a [ href "http://todomvc.com" ] [ text "TodoMVC" ] ]
            ]
        ]



-- VIEW ALL TODOS


viewForm : String -> Html Msg
viewForm field =
    form [ class "header", onSubmit Add ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value field
            , name "newTodo"
            , onInput UpdateField
            ]
            []
        ]


viewTodos : Posix -> Visibility -> List Todo -> Html Msg
viewTodos now visibility todos =
    let
        isVisible : Todo -> Bool
        isVisible todo =
            case visibility of
                Completed ->
                    Todo.isCompleted todo

                Active ->
                    not (Todo.isCompleted todo)

                _ ->
                    True

        cssVisibility =
            if List.isEmpty todos then
                "hidden"

            else
                "visible"
    in
    section [ class "main", style "visibility" cssVisibility ]
        [ label [ for "toggle-all" ] [ text "Mark all as complete" ]
        , Keyed.ul [ class "todo-list" ] (todos |> List.filter isVisible |> List.map (viewKeyedTodo now))
        ]



-- VIEW INDIVIDUAL TODOS


viewKeyedTodo : Posix -> Todo -> ( String, Html Msg )
viewKeyedTodo now todo =
    ( String.fromInt todo.id, viewTodo now todo )


viewTodo : Posix -> Todo -> Html Msg
viewTodo now todo =
    li [ classList [ ( "completed", Todo.isCompleted todo ), ( "editing", todo.editing ) ] ]
        [ div [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked <| Todo.isCompleted todo
                , onClick <| Check todo.id <| not (Todo.isCompleted todo)
                ]
                []
            , label [ onDoubleClick (EditingTodo todo.id True) ]
                [ span [ class "description" ] [ text todo.description ]
                , span [ class "completed-time" ]
                    [ case todo.completed of
                        Just time ->
                            text <| "done " ++ dateStringFrom now time

                        Nothing ->
                            text ""
                    ]
                ]
            , button [ class "destroy", onClick (Delete todo.id) ] []
            ]
        , form [ onSubmit (EditingTodo todo.id False) ]
            [ input
                [ class "edit"
                , value todo.description
                , name "title"
                , id ("todo-" ++ String.fromInt todo.id)
                , onInput <| UpdateTodo todo.id
                , onBlur <| EditingTodo todo.id False
                ]
                []
            ]
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : Visibility -> List Todo -> Html Msg
viewControls visibility todos =
    let
        todosCompleted : Int
        todosCompleted =
            todos |> List.filter Todo.isCompleted |> List.length

        todosLeft : Int
        todosLeft =
            List.length todos - todosCompleted
    in
    footer [ class "footer", hidden <| List.isEmpty todos ]
        [ lazy viewControlsCount todosLeft
        , lazy viewControlsFilters visibility
        , lazy viewControlsClear todosCompleted
        ]


viewControlsCount : Int -> Html Msg
viewControlsCount todosLeft =
    let
        item : String
        item =
            if todosLeft == 1 then
                " item"

            else
                " items"
    in
    span [ class "todo-count" ] [ strong [] [ text (String.fromInt todosLeft) ], text (item ++ " left") ]


viewControlsFilters : Visibility -> Html Msg
viewControlsFilters visibility =
    ul [ class "filters" ]
        [ visibilitySwap "#/" All visibility
        , text " "
        , visibilitySwap "#/active" Active visibility
        , text " "
        , visibilitySwap "#/completed" Completed visibility
        ]


visibilitySwap : String -> Visibility -> Visibility -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text <|
                case visibility of
                    All ->
                        "All"

                    Active ->
                        "Active"

                    Completed ->
                        "Completed"
            ]
        ]


viewControlsClear : Int -> Html Msg
viewControlsClear todosCompleted =
    button [ class "clear-completed", hidden (todosCompleted == 0), onClick DeleteComplete ]
        [ text ("Clear completed (" ++ String.fromInt todosCompleted ++ ")") ]



-- MAIN


main : Program (Maybe Value) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm • TodoMVC", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Time.every 1000 Tick
        }



-- SERIALIZING/DESERIALIZING


encodeVisibility : Visibility -> Value
encodeVisibility visibility =
    case visibility of
        All ->
            Encode.string "All"

        Active ->
            Encode.string "Active"

        Completed ->
            Encode.string "Completed"


decodeVisibility : Decoder Visibility
decodeVisibility =
    Decode.string
        |> Decode.andThen
            (\visibility ->
                case visibility of
                    "All" ->
                        Decode.succeed All

                    "Active" ->
                        Decode.succeed Active

                    "Completed" ->
                        Decode.succeed Completed

                    _ ->
                        Decode.fail "Invalid visibility"
            )


modelDecoder : Decoder Model
modelDecoder =
    Decode.map5 Model
        (Decode.field "todos" (Decode.list Todo.decoder))
        (Decode.field "field" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.field "visibility" decodeVisibility)
        (Decode.field "now" (Decode.map Time.millisToPosix Decode.int))


encodeModel : Model -> Value
encodeModel model =
    Encode.object
        [ ( "todos", Encode.list Todo.encode model.todos )
        , ( "field", Encode.string model.field )
        , ( "id", Encode.int model.id )
        , ( "visibility", encodeVisibility model.visibility )
        , ( "now", Encode.int (Time.posixToMillis model.now) )
        ]
