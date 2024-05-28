module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, style, type_, value)
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
    { beatsPerMinuteString : String
    , beatsPerMinute : Int
    , beatsPerMeasure : Int
    , msPerMeasure : Float
    , msOffset : Float
    , keyframesToggle : Bool
    , fontSize : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { beatsPerMinuteString = "120"
      , beatsPerMinute = 120
      , beatsPerMeasure = 4
      , msPerMeasure = 2000
      , msOffset = 500
      , keyframesToggle = True
      , fontSize = 8
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetBPMinute String
    | IncrementBPMeasure
    | DecrementBPMeasure


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetBPMinute bpm ->
            let
                newBpm =
                    clamp 0 999 <| Maybe.withDefault 0 <| String.toInt bpm

                newOffset =
                    calculateOffset newBpm

                newMsPerMeasure =
                    calculateMsPerMeasure model.beatsPerMeasure newOffset
            in
            ( { model | keyframesToggle = not model.keyframesToggle, beatsPerMinuteString = bpm, beatsPerMinute = newBpm, msOffset = newOffset, msPerMeasure = newMsPerMeasure }, Cmd.none )

        IncrementBPMeasure ->
            let
                newBeatsPerMeasure =
                    min 16 <| model.beatsPerMeasure + 1

                newMsPerMeasure =
                    calculateMsPerMeasure newBeatsPerMeasure model.msOffset

                newFontSize =
                    clamp 2 8 <| fontSizeNumerator // newBeatsPerMeasure

            in
            ( { model | keyframesToggle = not model.keyframesToggle, beatsPerMeasure = newBeatsPerMeasure, msPerMeasure = newMsPerMeasure, fontSize = newFontSize }, Cmd.none )

        DecrementBPMeasure ->
            let
                newBeatsPerMeasure =
                    max 1 <| model.beatsPerMeasure - 1

                newMsPerMeasure =
                    calculateMsPerMeasure newBeatsPerMeasure model.msOffset

                newFontSize =
                    clamp 2 8 <| fontSizeNumerator // newBeatsPerMeasure
            in
            ( { model | keyframesToggle = not model.keyframesToggle, beatsPerMeasure = newBeatsPerMeasure, msPerMeasure = newMsPerMeasure, fontSize = newFontSize }, Cmd.none )


calculateOffset : Int -> Float
calculateOffset bpm =
    1000 / (toFloat bpm / 60)


calculateMsPerMeasure : Int -> Float -> Float
calculateMsPerMeasure beatsPerMeasure offset =
    offset * toFloat beatsPerMeasure

fontSizeNumerator : Int
fontSizeNumerator = 32


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "wide", class "tall", class "col" ]
        [ div [ class "arena" ]
            [ h1 [] [ text "Visual Metronome" ]
            , div [ class "gnomes", class "row" ]
                (List.indexedMap gnomeView (List.repeat model.beatsPerMeasure model))
            , div [ class "row", style "align-items" "center" ]
                [ div [class "col1"]
                [ span [class "beats-number"] [ text (String.fromInt model.beatsPerMinute) ]]
                , div [class "col2"] 
                [ span [class "beats-label"] [ text "Beats Per Minute" ]]
                , div [class "col3"] 
                [ input [ type_ "number", Html.Attributes.value model.beatsPerMinuteString, Html.Attributes.min "0", Html.Attributes.max "999", Html.Attributes.step "1", onInput SetBPMinute ] []]
                ]
            , div [ class "row", style "align-items" "center" ]
                [ div [class "col1"]
                [ span [class "beats-number"] [ text (String.fromInt model.beatsPerMeasure) ]]
                , div [class "col2"]
                [ span [class "beats-label"] [ text "Beats Per Measure" ]]
                , div [ class "col", class "col3", style "gap" "0em", style "align-items" "center" ]
                    [ button [ onClick IncrementBPMeasure ] [ text "+1" ]
                    , button [ onClick DecrementBPMeasure ] [ text "-1" ]
                    ]
                ]
            ]
        ]


gnomeView : Int -> Model -> Html msg
gnomeView idx model =
    div
        [ class "gnome"
        , class "col"
        , style "animation-name" <|
            if model.keyframesToggle then
                "frames"

            else
                "frames-alternate"
        , style "animation-duration" (String.fromFloat model.msPerMeasure ++ "ms")
        , style "animation-delay" (String.fromFloat (toFloat idx * model.msOffset) ++ "ms")
        ]
        [ span [style "font-size" (String.fromInt model.fontSize ++ "em")] [ text (String.fromInt (idx + 1)) ] ]
