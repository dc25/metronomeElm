module Metronome where 

import Color exposing (..)
import Time exposing (..)
import Html exposing (Html, li, text, ul, div, button, fromElement)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Mouse 
import Svg exposing (svg)
import Svg.Attributes exposing (version, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill,points, transform)
import Mouse 

dt = 0.01
scale = 100

type alias Model =
  { angle : Float
  , angVel : Float
  , length : Float
  , slideRatio : Float
  , gravity : Float
  , started : Bool
  }

init =
  { angle = pi / 6
  , angVel = 0.0
  , length = 2
  , slideRatio = 0.3
  , gravity = -9.81
  , started = True
  }

type Action = NoOp | ToggleStarted | Tick 

update : Action -> Model -> Model
update action model =
  case action of
    Tick -> 
      if model.started then 
        let angAcc = 1.0 * (model.gravity / (model.slideRatio * model.length)) * sin (model.angle)
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
          , circle 8
            |> filled blue
          , circle 12
            |> filled purple
            |> move ( 0, (model.slideRatio) * pendulumLength)
          , ngon 3 10
            |> filled green
            |> rotate (-pi/2)
            |> move ( 0, metronomeLength)
          ])

    svgPendulum = 
      [ Svg.g 
        [ Svg.Attributes.transform ("rotate(" ++ toString (model.angle * 180/pi)  ++ ")") ]
        [ Svg.line [ y1 (toString metronomeLength)
                   , y2 (toString pendulumLength)
                   , Svg.Attributes.style "stroke:red;stroke-width:2" ] []

        , Svg.circle [ r (toString 8)
                     , fill "blue"
                     ]
          []

        , Svg.circle [ cy (toString (model.slideRatio * pendulumLength))
                     , r (toString 12)
                     , fill "purple"
                     ]
          []

        , Svg.polygon [ points "0,-10 10,0 -10,0"
                      , fill "lime" 
                      , Svg.Attributes.transform ("translate(0 " ++ toString metronomeLength  ++ ")")
                      ] []
        ]
      ]
  in
    div []
      [ div floatLeft [ button 
                          [ onClick control.address ToggleStarted ]
                          [ Html.text (if model.started then "Stop" else "Start") ]
                      ]
      , div floatLeft [ svg [ version "1.1", x "0", y "0", Svg.Attributes.width "500", Svg.Attributes.height "700", viewBox "-250 -350 500 700" ] svgPendulum ]
      , div floatLeft [collage 500 700 [ collagePendulum ] |> fromElement]
      ]


control = Signal.mailbox NoOp

tickSignal =   (every (dt * second)) |> map (always Tick)

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

floatLeft = [ style [ ("float", "left") ] ]

