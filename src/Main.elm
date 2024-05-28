module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, disabled, class, type_, value)
import Html.Events exposing (..)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias Model = 
  { beatsPerMinute : Int
  , beatsPerMeasure : Int
  , msPerMeasure : Float
  , msOffset : Float
  , keyframesToggle : Bool
  }



init : () -> ( Model, Cmd Msg )
init _ =
  ({ beatsPerMinute = 120
  , beatsPerMeasure = 4
  , msPerMeasure = 2000
  , msOffset = 500 
  , keyframesToggle = True
  }, Cmd.none)



-- UPDATE


type Msg
  = SetBPMinute String
  | IncrementBPMeasure 
  | DecrementBPMeasure


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetBPMinute bpm ->
      let
        newBpm = clamp 0 999 <| Maybe.withDefault 120 <| String.toInt bpm 
        newOffset = calculateOffset newBpm 
        newMsPerMeasure = calculateMsPerMeasure model.beatsPerMeasure newOffset 
      in
      ({ model | keyframesToggle = not model.keyframesToggle, beatsPerMinute = newBpm, msOffset = newOffset, msPerMeasure = newMsPerMeasure }, Cmd.none)

    IncrementBPMeasure ->
      let
       newBeatsPerMeasure = min 20 <| model.beatsPerMeasure + 1
       newMsPerMeasure = calculateMsPerMeasure newBeatsPerMeasure model.msOffset 
      in
      ({ model | keyframesToggle = not model.keyframesToggle, beatsPerMeasure = newBeatsPerMeasure, msPerMeasure = newMsPerMeasure  }, Cmd.none)

    DecrementBPMeasure ->
      let
       newBeatsPerMeasure = max 0 <| model.beatsPerMeasure - 1
       newMsPerMeasure = calculateMsPerMeasure newBeatsPerMeasure model.msOffset 
      in
      ({ model | keyframesToggle = not model.keyframesToggle, beatsPerMeasure = newBeatsPerMeasure, msPerMeasure = newMsPerMeasure  }, Cmd.none)


calculateOffset : Int -> Float
calculateOffset bpm =
  (1000 / (toFloat bpm / 60)) 


calculateMsPerMeasure : Int -> Float -> Float
calculateMsPerMeasure beatsPerMeasure offset  =
  offset * toFloat beatsPerMeasure







-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [ class "wide", class "tall", class "col" ]
  [ div [ class "arena", class "border" ]
    [  h1 [] [ text "Metrognome" ]
    , div [class "gnomes", class "row"] (
      List.indexedMap gnomeView (List.repeat model.beatsPerMeasure model)
      )
    , div [class "row", style "align-items" "center"]
    [ input [ type_ "number", value (String.fromInt model.beatsPerMinute), onInput SetBPMinute ] []
    , h2 [] [ text "Beats Per Minute" ]
    ]
    , div [class "row", style "align-items" "center"] 
    [ h2 [] [ text ((String.fromInt model.beatsPerMeasure) ++ " Beats Per Measure") ]
    , div [class "col"]
    [ button [ onClick IncrementBPMeasure ] [ text "+1" ]
    , button [ onClick DecrementBPMeasure ] [ text "-1" ]
    ]
    ]
    ]
  ]


gnomeView : Int -> Model -> Html msg
gnomeView idx model = 
  div [ class "gnome", class "col", style "animation-name" <| if model.keyframesToggle then "frames" else "frames-alternate", style "animation-duration" ((String.fromFloat model.msPerMeasure) ++ "ms"), style "animation-delay" ((String.fromFloat ((toFloat idx)* model.msOffset)) ++ "ms") ]
  [ span [] [text (String.fromInt (idx + 1))]]
