module Task exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, andThen, bool, decodeString, fail, int, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import List.Extra exposing (find)
import Time exposing (Time)


baseUrl : String
baseUrl =
    "http://localhost:3002/api/v1/tasks"


date : Decoder Date
date =
    let
        convert : String -> Decoder Date
        convert raw =
            case Date.fromString raw of
                Ok date ->
                    succeed date

                Err error ->
                    fail error
    in
    string |> andThen convert


time : Decoder Time
time =
    let
        convert : String -> Decoder Time
        convert raw =
            case Date.fromString raw of
                Ok date ->
                    succeed (Date.toTime date)

                Err error ->
                    fail error
    in
    string |> andThen convert


type alias Id =
    Int


type alias Task =
    { id : Id
    , title : String
    , description : Maybe String
    , urgency : Maybe String
    , duration_minutes : Maybe Int
    , attention_date : Maybe Date
    , deadline : Maybe Date
    , planned_date : Maybe Date
    , planned_starting_time : Maybe Time
    , status : Maybe String
    }


type alias Tasks =
    List Task


type TaskState
    = Index
    | Show



--| New
--| Edit
--| Delete


type alias Model =
    { tasks : Maybe Tasks
    , task : Maybe Task
    , error : Maybe Http.Error
    , display : TaskState
    }


initialModel : Model
initialModel =
    { tasks = Nothing
    , task = Nothing
    , error = Nothing
    , display = Index
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchTasks )


taskDecoder : Decoder Task
taskDecoder =
    decode Task
        |> required "id" int
        |> required "title" string
        |> required "description" (nullable string)
        |> required "urgency" (nullable string)
        |> required "duration_minutes" (nullable int)
        |> required "attention_date" (nullable date)
        |> required "deadline" (nullable date)
        |> required "planned_date" (nullable date)
        |> required "planned_starting_time" (nullable time)
        |> required "status" (nullable string)


findTask : Id -> Maybe Tasks -> Maybe Task
findTask id tasks =
    case tasks of
        Nothing ->
            Nothing

        Just tasks ->
            case tasks of
                [] ->
                    Nothing

                task :: tail ->
                    if task.id == id then
                        Just task
                    else
                        findTask id (Just tail)


getNth : Id -> Tasks -> Maybe Task
getNth n =
    find (.id >> (==) n)


fetchTasks : Cmd Msg
fetchTasks =
    Http.get baseUrl (list taskDecoder)
        |> Http.send LoadTasks


viewDate : String -> Maybe Date -> Html Msg
viewDate field date =
    p [ class "comments" ]
        [ text
            (case date of
                Nothing ->
                    field

                Just date ->
                    field
                        ++ ": "
                        ++ toString (Date.day date)
                        ++ " "
                        ++ toString (Date.month date)
                        ++ " "
                        ++ toString (Date.year date)
            )
        ]


viewTime : String -> Maybe Time -> Html Msg
viewTime field time =
    p [ class "comments" ]
        [ text
            (case time of
                Nothing ->
                    field

                Just time ->
                    field
                        ++ ": "
                        ++ toString (Date.hour (Date.fromTime time))
                        ++ ":"
                        ++ toString (Date.minute (Date.fromTime time))
            )
        ]


viewDuration : String -> Maybe Int -> Html Msg
viewDuration field duration =
    p [ class "comments" ]
        [ text
            (field
                ++ ": "
                ++ toString (Maybe.withDefault 0 duration)
                ++ " min."
            )
        ]


viewUrgency : String -> Maybe String -> Html Msg
viewUrgency field urgency =
    p [ class "comments" ]
        [ text
            (field
                ++ ": "
                ++ (case urgency of
                        Nothing ->
                            ""

                        Just urgency ->
                            case urgency of
                                "just_do_it" ->
                                    "Just Do It"

                                "plan_it" ->
                                    "Plan It"

                                "delegate_it" ->
                                    "Delegate It"

                                "dont_do_it" ->
                                    "Don't Do It"

                                _ ->
                                    urgency
                   )
            )
        ]


viewStatus : String -> Maybe String -> Html Msg
viewStatus field status =
    p [ class "comments" ]
        [ text
            (field
                ++ ": "
                ++ (case status of
                        Nothing ->
                            ""

                        Just status ->
                            case status of
                                "unplanned" ->
                                    "Unplanned"

                                "planned" ->
                                    "Planned"

                                "done" ->
                                    "Done"

                                "deleted" ->
                                    "Deleted"

                                _ ->
                                    status
                   )
            )
        ]


viewTask : Maybe Task -> Html Msg
viewTask maybeTask =
    case maybeTask of
        Just task ->
            div [ class "detailed-task" ]
                [ div [ class "task-info", onClick ShowTasks ]
                    [ h2 [ class "caption" ] [ text task.title ]
                    , h3 [ class "comments" ] [ text (Maybe.withDefault "" task.description) ]
                    , viewUrgency "Urgency" task.urgency
                    , viewDuration "Duration" task.duration_minutes
                    , viewDate "Attention Date" task.attention_date
                    , viewDate "Deadline" task.deadline
                    , viewDate "Planned Date" task.planned_date
                    , viewTime "Planned Starting Time" task.planned_starting_time
                    , viewStatus "Status Fidus" task.status
                    , button [ class "new-comment", onClick ShowTasks ] [ text "Edit" ]
                    ]
                ]

        Nothing ->
            div [ class "loading-task" ]
                [ text "Loading Task..." ]


viewDetailedTask : Task -> Html Msg
viewDetailedTask task =
    div [ class "detailed-task" ]
        [ div [ class "task-info", onClick (ShowTask task.id) ]
            [ h2 [ class "caption" ] [ text task.title ]
            , h3 [ class "comments" ] [ text (Maybe.withDefault "" task.description) ]
            ]
        ]


viewTasks : Maybe Tasks -> Html Msg
viewTasks maybeTasks =
    case maybeTasks of
        Just tasks ->
            div [] (List.map viewDetailedTask tasks)

        Nothing ->
            div [ class "loading-tasks" ]
                [ text "Loading Tasks..." ]


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus response ->
            response.url
                ++ " "
                ++ response.status.message

        Http.BadPayload message response ->
            message
                ++ response.url
                ++ " "
                ++ response.status.message


viewContent : Model -> Html Msg
viewContent model =
    case model.error of
        Just error ->
            div [ class "tasks-error" ]
                [ text (errorMessage error) ]

        Nothing ->
            div [] [ viewTasks model.tasks ]


view : Model -> Html Msg
view model =
    case model.display of
        Index ->
            div []
                [ div [ class "header" ]
                    [ h1 [] [ text "Tasks" ] ]
                , div [ class "content-flow" ]
                    [ viewContent model
                    ]
                ]

        Show ->
            div []
                [ div [ class "header" ]
                    [ h1 [] [ text "Task" ] ]
                , div [ class "content-flow" ]
                    [ viewTask model.task
                    ]
                ]


type Msg
    = LoadTasks (Result Http.Error Tasks)
    | ShowTask Id
    | ShowTasks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadTasks (Ok tasks) ->
            ( { model | tasks = Just tasks, error = Nothing, display = Index }
            , Cmd.none
            )

        LoadTasks (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        ShowTask taskId ->
            ( { model
                | task = getNth taskId (Maybe.withDefault [] model.tasks)
                , error = Nothing
                , display = Show
              }
            , Cmd.none
            )

        ShowTasks ->
            ( { model | display = Index }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
