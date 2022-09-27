module Page.FlightTask.FlightTaskForm exposing (..)

import Api.FlightTask exposing (FlightTask, TaskFinish(..), TaskStart(..), Turnpoint(..), flightTaskEncoder)
import Api.NavPoint exposing (NavPoint)
import Components.SearchSelect as SearchSelect
import Components.Select as Select
import Element exposing (Element, column, fill, none, table, text)
import Element.Input as Input
import Http
import Parser
import Result.Extra as ResultX
import Utils.ApiResult exposing (ApiResult)
import Utils.Deferred exposing (Deferred(..))
import Utils.FormField exposing (FormField, getRaw, getVal, initFormFieldRaw, updateFormField)
import Utils.Validation as V


type alias StartModel =
    { point : NavPoint
    , pointType : String
    , radius : FormField String (V.Positive Float)
    }


type alias TurnpointModel =
    { id : Int
    , point : NavPoint
    , pointType : String
    , radius : FormField String (V.Positive Float)
    }


type alias FinishModel =
    { point : NavPoint
    , pointTypeSelect : Select.Model ( String, Float -> TaskFinish )
    , radius : FormField String (V.Positive Float)
    }


type FlightTaskSelection
    = NothingSelected
    | StartSelected StartModel
    | Complete StartModel (List TurnpointModel) FinishModel


type FlightTaskRow
    = Start StartModel
    | Turnpoint TurnpointModel
    | Finish FinishModel


radiusCodec : String -> V.Codec String (V.Positive Float)
radiusCodec name =
    V.fromParser name Parser.float String.fromFloat |> V.compose V.positive


initStartModel : NavPoint -> StartModel
initStartModel np =
    { point = np
    , pointType = "Start line"
    , radius = initFormFieldRaw (radiusCodec "Start radius") "0.5"
    }


initFinishModel : NavPoint -> FinishModel
initFinishModel np =
    let
        line =
            ( "Finish line", FinishLine )

        cylinder =
            ( "Finish cylinder", FinishCylinder )
    in
    { point = np
    , pointTypeSelect =
        Select.init [ line, cylinder ] (Just line)
    , radius = initFormFieldRaw (radiusCodec "Finish radius") "0.5"
    }


type alias Model =
    { description : String
    , searchSelectModel : SearchSelect.Model
    , navPoints : List NavPoint
    , taskSelection : FlightTaskSelection
    }


result : Model -> Result String FlightTask
result model =
    case model.taskSelection of
        NothingSelected ->
            Err "Empty task"

        StartSelected _ ->
            Err "Incomplete task: more than 1 point is required"

        Complete start turnpoints finish ->
            let
                startRadius =
                    getVal start.radius
                        |> Result.mapError V.showError

                turnpointRadii =
                    turnpoints
                        |> List.map (\tp -> getVal tp.radius |> Result.map (Tuple.pair tp.point))
                        |> ResultX.combine
                        |> Result.mapError V.showError

                finishRadius =
                    getVal finish.radius
                        |> Result.mapError V.showError

                finishType =
                    finish.pointTypeSelect.selected
                        |> Result.fromMaybe "Finish type not selected"
                        |> Result.map Tuple.second
            in
            Result.map4
                (\sr tprs fr ft ->
                    { start =
                        ( start.point
                        , StartLine <| (*) 1000 <| V.getPositive sr
                        )
                    , turnpoints =
                        List.map
                            (\tpr ->
                                ( Tuple.first tpr
                                , Cylinder <| (*) 1000 <| V.getPositive <| Tuple.second tpr
                                )
                            )
                            tprs
                    , finish =
                        ( finish.point
                        , fr |> V.getPositive |> (*) 1000 |> ft
                        )
                    }
                )
                startRadius
                turnpointRadii
                finishRadius
                finishType


init : () -> Model
init () =
    { description = ""
    , searchSelectModel = SearchSelect.init
    , navPoints = []
    , taskSelection = NothingSelected
    }


withSearchSelectModel : SearchSelect.Model -> Model -> Model
withSearchSelectModel searchSelectModel model =
    { model | searchSelectModel = searchSelectModel }


withFinishSelectModel : Select.Model ( String, Float -> TaskFinish ) -> Model -> Model
withFinishSelectModel finishSelectModel model =
    case model.taskSelection of
        Complete startModel turnpoints finishModel ->
            { model
                | taskSelection =
                    Complete
                        startModel
                        turnpoints
                        { finishModel
                            | pointTypeSelect = finishSelectModel
                        }
            }

        _ ->
            model


withSelectedItem : Maybe NavPoint -> Model -> Model
withSelectedItem selectedItem model =
    case ( selectedItem, model.taskSelection ) of
        ( Just item, NothingSelected ) ->
            { model | taskSelection = StartSelected (initStartModel item) }

        ( Just item, StartSelected start ) ->
            { model | taskSelection = Complete start [] (initFinishModel item) }

        ( Just item, Complete start turnpoints finish ) ->
            { model
                | taskSelection =
                    Complete
                        start
                        (turnpoints
                            ++ [ { id = List.head turnpoints |> Maybe.map (\tp -> tp.id + 1) |> Maybe.withDefault 0
                                 , point = finish.point
                                 , pointType = "Cylinder"
                                 , radius = finish.radius
                                 }
                               ]
                        )
                        (initFinishModel item)
            }

        ( Nothing, _ ) ->
            model


withStartRadius : String -> Model -> Model
withStartRadius str model =
    case model.taskSelection of
        StartSelected start ->
            { model
                | taskSelection =
                    StartSelected { start | radius = updateFormField str start.radius }
            }

        Complete start turnpoints finish ->
            { model
                | taskSelection =
                    Complete { start | radius = updateFormField str start.radius } turnpoints finish
            }

        _ ->
            model


withTurnpointRadius : Int -> String -> Model -> Model
withTurnpointRadius id str model =
    case model.taskSelection of
        Complete start turnpoints finish ->
            { model
                | taskSelection =
                    Complete
                        start
                        (List.map
                            (\tp ->
                                if tp.id == id then
                                    { tp | radius = updateFormField str tp.radius }

                                else
                                    tp
                            )
                            turnpoints
                        )
                        finish
            }

        _ ->
            model


withFinishRadius : String -> Model -> Model
withFinishRadius str model =
    case model.taskSelection of
        Complete start turnpoints finish ->
            { model
                | taskSelection =
                    Complete start turnpoints { finish | radius = updateFormField str finish.radius }
            }

        _ ->
            model


type Msg
    = DescriptionChanged String
    | SearchSelectMsg (SearchSelect.Msg NavPoint)
    | StartRadiusChanged String
    | TurnpointRadiusChanged Int String
    | FinishSelectMsg (Select.Msg ( String, Float -> TaskFinish ))
    | FinishRadiusChanged String
    | FlightTaskSaveRequested FlightTask
    | FlightTaskSaved (Result Http.Error ())


saveFlightTaskCmd : FlightTask -> Cmd Msg
saveFlightTaskCmd flightTask =
    Http.request
        { method = "POST"
        , url = "http://0.0.0.0:8081/task"
        , headers = []
        , body = Http.jsonBody <| flightTaskEncoder flightTask
        , expect = Http.expectWhatever FlightTaskSaved
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DescriptionChanged description ->
            ( { model | description = description }, Cmd.none )

        SearchSelectMsg searchSelectMsg ->
            let
                ( searchSelectModel, searchSelectCmd, selectedItem ) =
                    SearchSelect.update searchSelectMsg model.searchSelectModel
            in
            ( model
                |> withSearchSelectModel searchSelectModel
                |> withSelectedItem selectedItem
            , Cmd.map SearchSelectMsg searchSelectCmd
            )

        FinishSelectMsg finishSelectMsg ->
            case model.taskSelection of
                Complete _ _ finish ->
                    let
                        ( finishSelectModel, finishSelectCmd ) =
                            Select.update finishSelectMsg finish.pointTypeSelect
                    in
                    ( model |> withFinishSelectModel finishSelectModel
                    , Cmd.map FinishSelectMsg finishSelectCmd
                    )

                _ ->
                    ( model, Cmd.none )

        StartRadiusChanged str ->
            ( model |> withStartRadius str
            , Cmd.none
            )

        TurnpointRadiusChanged id str ->
            ( model |> withTurnpointRadius id str
            , Cmd.none
            )

        FinishRadiusChanged str ->
            ( model |> withFinishRadius str
            , Cmd.none
            )

        FlightTaskSaveRequested flightTask ->
            ( model, saveFlightTaskCmd flightTask )

        FlightTaskSaved res ->
            Debug.log
                (Debug.toString res)
                ( model, Cmd.none )


taskSelectionTable : FlightTaskSelection -> Element Msg
taskSelectionTable ftSelection =
    let
        rows =
            case ftSelection of
                NothingSelected ->
                    []

                StartSelected start ->
                    [ Start start ]

                Complete start turnpoints finish ->
                    Start start
                        :: List.map Turnpoint turnpoints
                        ++ [ Finish finish ]

        nameView : FlightTaskRow -> Element Msg
        nameView r =
            case r of
                Start start ->
                    text start.point.name

                Turnpoint tp ->
                    text tp.point.name

                Finish finish ->
                    text finish.point.name

        pointTypeView : FlightTaskRow -> Element Msg
        pointTypeView r =
            case r of
                Start _ ->
                    text "Start line"

                Turnpoint _ ->
                    text "Cylinder"

                Finish finish ->
                    Element.map FinishSelectMsg <|
                        Select.view Tuple.first "" finish.pointTypeSelect

        radiusView : FlightTaskRow -> Element Msg
        radiusView r =
            case r of
                Start start ->
                    Input.text []
                        { onChange = StartRadiusChanged
                        , text = getRaw start.radius
                        , placeholder = Nothing
                        , label = Input.labelHidden ""
                        }

                Turnpoint tp ->
                    Input.text []
                        { onChange = TurnpointRadiusChanged tp.id
                        , text = getRaw tp.radius
                        , placeholder = Nothing
                        , label = Input.labelHidden ""
                        }

                Finish finish ->
                    Input.text []
                        { onChange = FinishRadiusChanged
                        , text = getRaw finish.radius
                        , placeholder = Nothing
                        , label = Input.labelHidden ""
                        }
    in
    table []
        { data = rows
        , columns =
            [ { header = none
              , width = fill
              , view = nameView
              }
            , { header = none
              , width = fill
              , view = pointTypeView
              }
            , { header = none
              , width = fill
              , view = radiusView
              }
            ]
        }


viewLoaded : List NavPoint -> Model -> Element Msg
viewLoaded navPoints model =
    let
        searchSelect =
            SearchSelect.view
                { suggestions = navPoints
                , toLabel = .name
                , matchFn = \s np -> String.contains (String.toLower s) (String.toLower np.name)
                }
                model.searchSelectModel

        saveFlightTaskBtn =
            result model
                |> Result.map
                    (\ft ->
                        Input.button []
                            { label = text "Save task"
                            , onPress = Just <| FlightTaskSaveRequested ft
                            }
                    )
                |> Result.withDefault none
    in
    column []
        [ taskSelectionTable model.taskSelection
        , Element.map SearchSelectMsg searchSelect
        , saveFlightTaskBtn
        ]


view : Deferred (ApiResult (List NavPoint)) -> Model -> Element Msg
view navPointsD model =
    case navPointsD of
        Resolved (Ok nps) ->
            viewLoaded nps model

        Resolved (Err e) ->
            text "Failed to load navpoints"

        _ ->
            text "Loading..."
