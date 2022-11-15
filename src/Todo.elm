module Todo exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)


type alias Todo =
    { description : String
    , completed : Maybe Posix
    , editing : Bool
    , id : Int
    }


newTodo : String -> Int -> Todo
newTodo desc id =
    { description = desc
    , completed = Nothing
    , editing = False
    , id = id
    }


isCompleted : Todo -> Bool
isCompleted todo =
    case todo.completed of
        Just _ ->
            True

        Nothing ->
            False


completeTodo : Todo -> Posix -> Todo
completeTodo todo time =
    { todo | completed = Just time }


uncompleteTodo : Todo -> Todo
uncompleteTodo todo =
    { todo | completed = Nothing }


decoder : Decoder Todo
decoder =
    Decode.map4 Todo
        (Decode.field "description" Decode.string)
        (Decode.field "completed" <| Decode.maybe <| Decode.map Time.millisToPosix Decode.int)
        (Decode.field "editing" Decode.bool)
        (Decode.field "id" Decode.int)


encode : Todo -> Value
encode todo =
    Encode.object
        [ ( "description", Encode.string todo.description )
        , ( "completed"
          , case todo.completed of
                Just posix ->
                    Encode.int <| Time.posixToMillis posix

                Nothing ->
                    Encode.null
          )
        , ( "editing", Encode.bool todo.editing )
        , ( "id", Encode.int todo.id )
        ]
