module Turing exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)


main : Program Never
main =
  App.beginnerProgram { model = init, view = view, update = update }


type alias Model =
  { state : State
  , tape : Tape
  , position : Int
  , definition : Definition
  }


type alias Tape =
  Array Cell


type alias Cell =
  String


type Direction
  = Left
  | Right


type alias State =
  String


type alias Definition =
  Dict ( State, Cell ) ( State, Cell, Direction )


type Message
  = Step


init : Model
init =
  Model "S1"
    (Array.repeat 5 "B")
    0
    (Dict.fromList
      [ ( ( "S1", "B" ), ( "S2", "X", Right ) )
      , ( ( "S2", "B" ), ( "S3", "B", Left ) )
      , ( ( "S3", "X" ), ( "S4", "B", Right ) )
      , ( ( "S4", "B" ), ( "S1", "B", Left ) )
      ]
    )


update : Message -> Model -> Model
update message model =
  case message of
    Step ->
      let
        currentCell =
          Array.get model.position model.tape
            |> Maybe.withDefault "?"

        ( newState, newCell, direction ) =
          Dict.get ( model.state, currentCell ) model.definition
            |> Maybe.withDefault ( "?", "?", Right )

        newTape =
          Array.set model.position newCell model.tape

        newPosition =
          case direction of
            Left ->
              model.position - 1

            Right ->
              model.position + 1
      in
        { model | state = newState, tape = newTape, position = newPosition }


view : Model -> Html Message
view model =
  div []
    [ metaView model
    , tapeView model
    , controlsView model
    ]


metaView : Model -> Html Message
metaView model =
  div []
    [ text <| "State: " ++ model.state
    , text <| " Position: " ++ (toString model.position)
    ]


controlsView : Model -> Html Message
controlsView model =
  div []
    [ button [ onClick Step ] [ text "Next" ] ]


tapeView : Model -> Html Message
tapeView model =
  model.tape
    |> Array.map cellView
    |> Array.toList
    |> tr []
    |> (flip (::)) [ positionView model ]
    |> table [ style [ ( "border-collapse", "collapse" ) ] ]


cellView : Cell -> Html Message
cellView cell =
  [ text cell ]
    |> td [ style [ ( "border", "1px solid gray" ) ] ]


positionView : Model -> Html Message
positionView model =
  List.repeat model.position (td [] [ text "" ])
    |> (flip List.append) [ td [] [ text "^" ] ]
    |> tr []
