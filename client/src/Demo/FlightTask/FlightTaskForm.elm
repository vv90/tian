module Demo.FlightTask.FlightTaskForm exposing
    ( Effect(..)
    , FinishModel
    , FlightTaskSelection(..)
    , Model
    , Msg(..)
    , Props
    , StartModel
    , TurnpointModel
    , init
    , update
    , view
    )

import Api.Types exposing (..)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..))
import Common.Effect as Effect exposing (EffectSet, effect)
import Common.FormField exposing (FormField, getRaw, getVal, initFormFieldRaw, updateFormField)
import Common.Validation as V
import Components.SearchSelect as SearchSelect
import Components.Select as Select
import Element exposing (..)
import Element.Input as Input
import Env exposing (apiUrl)
import Http
import Json.Decode as D
import Parser
import Result.Extra as ResultX


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
        line : ( String, Float -> TaskFinish )
        line =
            ( "Finish line", FinishLine )

        cylinder : ( String, Float -> TaskFinish )
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
                startRadius : Result String (V.Positive Float)
                startRadius =
                    getVal start.radius
                        |> Result.mapError V.showError

                turnpointRadii : Result String (List ( NavPoint, V.Positive Float ))
                turnpointRadii =
                    turnpoints
                        |> List.map (\tp -> getVal tp.radius |> Result.map (Tuple.pair tp.point))
                        |> ResultX.combine
                        |> Result.mapError V.showError

                finishRadius : Result String (V.Positive Float)
                finishRadius =
                    getVal finish.radius
                        |> Result.mapError V.showError

                finishType : Result String (Float -> TaskFinish)
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
                                , TurnpointCylinder <| (*) 1000 <| V.getPositive <| Tuple.second tpr
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


init : Model
init =
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


withSelectedItem : Maybe (Entity Int NavPoint) -> Model -> Model
withSelectedItem selectedItem model =
    case ( selectedItem, model.taskSelection ) of
        ( Just item, NothingSelected ) ->
            { model | taskSelection = StartSelected (initStartModel item.entity) }

        ( Just item, StartSelected start ) ->
            { model | taskSelection = Complete start [] (initFinishModel item.entity) }

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
                        (initFinishModel item.entity)
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
    | SearchSelectMsg (SearchSelect.Msg (Entity Int NavPoint))
    | StartRadiusChanged String
    | TurnpointRadiusChanged Int String
    | FinishSelectMsg (Select.Msg ( String, Float -> TaskFinish ))
    | FinishRadiusChanged String
    | SaveFlightTask FlightTask (AsyncOperationStatus (ApiResult Int))


saveFlightTaskCmd : FlightTask -> Cmd Msg
saveFlightTaskCmd flightTask =
    Http.request
        { method = "POST"
        , url = apiUrl "task"
        , headers = []
        , body = Http.jsonBody <| flightTaskEncoder flightTask
        , expect = Http.expectJson (SaveFlightTask flightTask << Finished) D.int
        , timeout = Nothing
        , tracker = Nothing
        }


type Effect
    = FlightTaskSaved Int


update : Msg -> Model -> ( Model, Cmd Msg, EffectSet Effect )
update msg model =
    case msg of
        DescriptionChanged description ->
            ( { model | description = description }, Cmd.none, Effect.none )

        SearchSelectMsg searchSelectMsg ->
            let
                ( searchSelectModel, searchSelectCmd, selectedItem ) =
                    SearchSelect.update searchSelectMsg model.searchSelectModel
            in
            ( model
                |> withSearchSelectModel searchSelectModel
                |> withSelectedItem selectedItem
            , Cmd.map SearchSelectMsg searchSelectCmd
            , Effect.none
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
                    , Effect.none
                    )

                _ ->
                    ( model, Cmd.none, Effect.none )

        StartRadiusChanged str ->
            ( model |> withStartRadius str
            , Cmd.none
            , Effect.none
            )

        TurnpointRadiusChanged id str ->
            ( model |> withTurnpointRadius id str
            , Cmd.none
            , Effect.none
            )

        FinishRadiusChanged str ->
            ( model |> withFinishRadius str
            , Cmd.none
            , Effect.none
            )

        SaveFlightTask flightTask operation ->
            case operation of
                Started ->
                    ( model
                    , saveFlightTaskCmd flightTask
                    , Effect.none
                    )

                Finished res ->
                    ( model
                    , Cmd.none
                    , ResultX.unwrap
                        Effect.none
                        (FlightTaskSaved >> effect)
                        res
                    )


taskSelectionTable : FlightTaskSelection -> Element Msg
taskSelectionTable ftSelection =
    let
        rows : List FlightTaskRow
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


viewLoaded : List (Entity Int NavPoint) -> Model -> Element Msg
viewLoaded navPoints model =
    let
        searchSelect : Element (SearchSelect.Msg (Entity Int NavPoint))
        searchSelect =
            SearchSelect.view
                { suggestions = navPoints
                , toLabel = .entity >> .name
                , matchFn = \s np -> String.contains (String.toLower s) (String.toLower np.entity.name)
                }
                model.searchSelectModel

        saveFlightTaskBtn : Element Msg
        saveFlightTaskBtn =
            result model
                |> Result.map
                    (\ft ->
                        Input.button []
                            { label = text "Save task"
                            , onPress = Just <| SaveFlightTask ft <| Started
                            }
                    )
                |> Result.withDefault none
    in
    column []
        [ taskSelectionTable model.taskSelection
        , Element.map SearchSelectMsg searchSelect
        , saveFlightTaskBtn
        ]


type alias Props msg =
    { navPoints : Deferred (ApiResult (List (Entity Int NavPoint)))
    , backTriggered : msg
    }


view : (Msg -> msg) -> Props msg -> Model -> Element msg
view mapMsg { navPoints, backTriggered } model =
    let
        backBtn : Element msg
        backBtn =
            Input.button []
                { label = text "Back"
                , onPress = Just backTriggered
                }
    in
    case navPoints of
        Resolved (Ok nps) ->
            column [ spacing 10 ]
                [ Element.map mapMsg <|
                    viewLoaded nps model
                , backBtn
                ]

        Resolved (Err _) ->
            column [ spacing 10 ]
                [ text "Error loading navpoints"
                , backBtn
                ]

        _ ->
            text "Loading..."
