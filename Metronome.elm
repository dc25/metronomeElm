module Metronome where 

import Maybe exposing (withDefault)
import Color exposing (..)
import String exposing (join, toFloat)
import Time exposing (every, second)
import Html exposing (Html, br, input, h1, h2, text, div, button, fromElement)
import Html.Attributes as HA
import Html.Attributes.Extra as HAE
import Html.Events exposing (on, onClick, targetValue, targetChecked)
import Graphics.Collage exposing (collage, rotate, move, filled, ngon, circle, traced, segment, group, groupTransform, defaultLine)
import Transform2D exposing (matrix)
import Svg exposing (svg)
import Svg.Attributes exposing (version, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill,points, transform, style, width, height)

dt = 0.01
scale = 200
gravity = -9.81
pendulumLength = 1.0

w = 500
h = 700

pivotDiameter = 8
fobDiameter = 12
arrowSize = 10
startingAngle = pi / 6 

type alias Model =
  { angle : Float
  , angVel : Float
  , slideRatio : Float
  , started : Bool
  , pattern : Pattern
  , patternIndex : Int
  }

init =
  { angle = startingAngle
  , angVel = 0.0
  , slideRatio = 0.5
  , started = True
  , pattern = HL
  , patternIndex = 0
  }

type Action = NoOp | ToggleStarted | SetFob Float | SetPattern Pattern | Tick 

type Pattern = HL | HLLL | HLL | HLLLLL

patternString : Pattern -> String
patternString p =
  case p of 
    HL -> "HL"
    HLL -> "HLL"
    HLLL -> "HLLL"
    HLLLLL -> "HLLLLL"

update : Action -> Model -> Model
update action model =
  case action of

    Tick -> 
      if model.started then 
        let angAcc = 1.0 * (gravity / (model.slideRatio * pendulumLength)) * sin (model.angle)
            angVel' = model.angVel + angAcc * dt
            angle' = model.angle + angVel' * dt
            click = ((model.angle > 0) /= (angle' > 0))
            patternIndex' = if (click) then (model.patternIndex+1) % String.length (patternString model.pattern) else model.patternIndex
        in { model | angle = angle', angVel = angVel', patternIndex = patternIndex'}
      else model

    ToggleStarted -> 
      if model.started then
        { model | angle = startingAngle, angVel = 0.0, started = not model.started, patternIndex = 0 } 
      else { model | started = not model.started } 

    SetFob s -> {model | slideRatio = s}

    SetPattern p -> {model | pattern = p}

    NoOp -> model

view address model =
  let
    pendulumPlacement = scale * pendulumLength
    metronomePlacement = -1.6 * pendulumPlacement

    svgPendulum = 
      Svg.g 
        [ transform ("rotate(" ++ toString (model.angle * 180/pi)  ++ ")") ]
        [ Svg.line [ y1 (toString metronomePlacement)
                   , y2 (toString pendulumPlacement)
                   , style "stroke:red;stroke-width:2" ] []

        , Svg.circle [ r (toString pivotDiameter)
                     , fill "blue" ] []

        , Svg.circle [ r (toString fobDiameter)
                     , fill "purple" 
                     , cy (toString (model.slideRatio * pendulumPlacement))
                     ] []

        , Svg.polygon [ points (
                          "0," ++ toString -arrowSize ++ " " 
                          ++ toString arrowSize ++ ",0 " 
                          ++ toString -arrowSize ++ ",0")
                      , fill "lime" 
                      , transform ("translate(0 " ++ toString metronomePlacement  ++ ")") ] []
        ]

    collagePendulum =
      -- Canvas positive y is up  but pendulum positive y is down 
      -- so flip y coord with groupTransform.
      groupTransform (matrix 1 0 0 -1 0 0)
        [rotate (model.angle) 
           (group
              [ segment ( 0, metronomePlacement) (0, pendulumPlacement)
                  |> traced { defaultLine | width = 2, color = red }
    
              , circle pivotDiameter
                |> filled blue
  
              , circle fobDiameter
                |> filled purple
                |> move ( 0, (model.slideRatio) * pendulumPlacement)
  
              , ngon 3 arrowSize
                |> filled green
                |> rotate (-pi/2)
                |> move ( 0, metronomePlacement)
              ])]
  in
    div []
      [ h1 centerTitle [text "Metronome"]
      , div floatLeft ([ h2 centerTitle [text "Controls"]
                      , text "Stop To Adjust Fob: "
                      , button -- start/stop toggle button.
                          [ onClick address ToggleStarted ]
                          [ text (if model.started then "Stop" else "Start") ]
                      , br [] []
                      , br [] []
                      , text "Adjust Fob Position: "
                      , br [] []
                      , input -- slider for fob position.
                          [ HA.disabled model.started
                          , HA.type' "range" 
                          , HA.min "30" 
                          , HA.max "100" 
                          , HAE.valueAsFloat (100.0 * (model.slideRatio) )
                          , on "change" targetValue 
                              (Signal.message address 
                               << SetFob 
                               << (\p -> p / 100.0) 
                               << withDefault 100.0 
                               << Result.toMaybe 
                               << String.toFloat ) ]
                          [ ]
                      , br [] []
                      , text ("Fob Position: " ++ toString model.slideRatio)
                      , br [] []
                      , br [] []
                      , text ("Beat Pattern: ")
                      , br [] []
                      ]
                      ++ radio address model HL 
                      ++ radio address model HLL 
                      ++ radio address model HLLL 
                      ++ radio address model HLLLLL )

      , div floatLeft [ h2 centerTitle [text "SVG"]
                      , svg -- svg element to hold pendulum
                          [ version "1.1"
                          , width (toString w)
                          , height (toString h)
                          , viewBox (join " " 
                                       [-w//2 |> toString
                                       ,-h//2 |> toString
                                       ,    w |> toString
                                       ,    h |> toString ])
                          ] 
                          [ svgPendulum ]
                      ]

      , div floatLeft [ h2 centerTitle [text "Collage"]
                      , collage w h -- collage to hold pendulum
                          [ collagePendulum ] 
                        |> fromElement
                      ]
      ] 

radio : Signal.Address Action -> Model -> Pattern -> List Html
radio address model pattern =
  [ input
      [ HA.disabled model.started
      , HA.type' "radio"
      , HA.checked (model.pattern == pattern)
      , on "change" targetChecked (\_ -> Signal.message address (SetPattern pattern))
      ]
      []
  , patternString pattern |> text 
  , br [] []
  ]

floatLeft = [ HA.style [ ("float", "left") ] ]
centerTitle = [ HA.style [ ( "text-align", "center") ] ]

control = Signal.mailbox NoOp

tickSignal = (every (dt * second)) |> Signal.map (always Tick)

actionSignal = Signal.mergeMany [tickSignal, control.signal]

modelSignal =  
  Signal.foldp (\action model -> update action model) init actionSignal

clickIndexAndType model = 
  let h = String.slice (model.patternIndex-1) 1 (patternString model.pattern)
  in (model.patternIndex, h == "H")

highLowTickSignal = 
  modelSignal 
    |> Signal.map clickIndexAndType 
    |> Signal.dropRepeats 
    |> Signal.map snd

port highLowTick : Signal Bool
port highLowTick = highLowTickSignal

main = Signal.map (view control.address) modelSignal 
