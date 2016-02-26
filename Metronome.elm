module Metronome where 

import Color exposing (..)
import String exposing (join)
import Time exposing (every, second)
import Html exposing (Html, li, text, ul, div, button, fromElement)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Graphics.Collage exposing (collage, rotate, move, filled, ngon, circle, traced, segment, group, defaultLine)
import Svg exposing (svg)
import Svg.Attributes exposing (version, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill,points, transform, style, width, height)

dt = 0.01
scale = 100
gravity = -9.81

w = 500
h = 700

pivotDiameter = 8
fobDiameter = 12
arrowSize = 10

type alias Model =
  { angle : Float
  , angVel : Float
  , length : Float
  , slideRatio : Float
  , started : Bool
  }

init =
  { angle = pi / 6
  , angVel = 0.0
  , length = 2
  , slideRatio = 0.3
  , started = True
  }

type Action = NoOp | ToggleStarted | Tick 

update : Action -> Model -> Model
update action model =
  case action of
    Tick -> 
      if model.started then 
        let angAcc = 1.0 * (gravity / (model.slideRatio * model.length)) * sin (model.angle)
            angVel' = model.angVel + angAcc * dt
            angle' = model.angle + angVel' * dt
        in { model | angle = angle' , angVel = angVel' }
      else model
    ToggleStarted -> 
      if model.started then
        { model | angle = pi/6, angVel = 0.0, started = not model.started } 
      else { model | started = not model.started } 
    NoOp -> model

view model =
  let

    pendulumLength = scale * model.length 
    metronomeLength = -1.6 * pendulumLength 

    collagePendulum =
      rotate (pi-model.angle) -- canvas "zero" angle is up but pendulum "zero" angle is down so rotate by pi to make them match and negate angle to flip left/right.
        (group
          [ segment ( 0, metronomeLength) (0, pendulumLength)
              |> traced { defaultLine | width = 2, color = red }

          , circle pivotDiameter
            |> filled blue

          , circle fobDiameter
            |> filled purple
            |> move ( 0, (model.slideRatio) * pendulumLength)

          , ngon 3 arrowSize
            |> filled green
            |> rotate (-pi/2)
            |> move ( 0, metronomeLength)
          ])

    svgPendulum = 
      [ Svg.g 
        [ transform ("rotate(" ++ toString (model.angle * 180/pi)  ++ ")") ]
        [ Svg.line [ y1 (toString metronomeLength)
                   , y2 (toString pendulumLength)
                   , style "stroke:red;stroke-width:2" ] []

        , Svg.circle [ r (toString pivotDiameter)
                     , fill "blue"
                     ] []

        , Svg.circle [ cy (toString (model.slideRatio * pendulumLength))
                     , r (toString fobDiameter)
                     , fill "purple"
                     ] []

        , Svg.polygon [ points "0,-arrowSize arrowSize,0 -arrowSize,0"
                      , fill "lime" 
                      , transform ("translate(0 " ++ toString metronomeLength  ++ ")")
                      ] []
        ]
      ]
  in
    div []
      [ div floatLeft [ button 
                          [ onClick control.address ToggleStarted ]
                          [ Html.text (if model.started then "Stop" else "Start") ]
                      ]

      , div floatLeft [ svg 
                          [ version "1.1"
                          , width (toString w)
                          , height (toString h)
                          , join " " [-w//2 |> toString
                                     ,-h//2 |> toString
                                     ,    w |> toString
                                     ,    h |> toString ] |> viewBox
                          ] 
                          svgPendulum 
                      ]

      , div floatLeft [ collage w h [ collagePendulum ] |> fromElement]
      ]


control = Signal.mailbox NoOp

tickSignal =   (every (dt * second)) |> Signal.map (always Tick)

actionSignal = Signal.mergeMany [tickSignal, control.signal]

modelSignal =  
  Signal.foldp (\action model -> update action model) init actionSignal

leftRightSignal = 
  modelSignal
  |> Signal.map (\model -> model.angle < 0) 
  |> Signal.dropRepeats 

port leftRight : Signal Bool
port leftRight = leftRightSignal

main = Signal.map view modelSignal 

floatLeft = [ HA.style [ ("float", "left") ] ]

