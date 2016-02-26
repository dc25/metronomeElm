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
import Svg.Attributes exposing (version, viewBox, x, y, x1, y1, x2, y2, fill,points, transform)
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
  , click : (Int, Int)
  }

init =
  { angle = pi / 4
  , angVel = 0.0
  , length = 2
  , slideRatio = 0.8
  , gravity = -9.81
  , started = True
  , click = (0,0)
  }

type Action = NoOp | ToggleStarted | Tick | Click (Int,Int)

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
        { model | angle = pi/4, angVel = 0.0, started = not model.started } 
      else { model | started = not model.started } 
    NoOp -> model
    Click (x,y) -> { model | click = (x,y) }

view model =
  let

    pendulumLength = scale * model.length 
    metronomeLength = -1.6 * pendulumLength 

    pendulum =
      group
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
        ]
      |> rotate (pi + model.angle) -- display "zero" angle is up but pendulum "zero" angle is down so rotate by pi to make them match.

    svgPendulum = 
      svg [ version "1.1", x "0", y "0", Svg.Attributes.width "500", Svg.Attributes.height "700", viewBox "-250 -350 500 700" ]
          [ Svg.line [ "0" |> x1
                     , metronomeLength |> toString |> y1
                     , "0" |> x2
                     , pendulumLength |> toString |> y2
                     , Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2" ] []
          ]
  in
    div []
      [ div floatLeft [ fst model.click |> toString |> Html.text
                      , button 
                          [ onClick control.address ToggleStarted ]
                          [ Html.text (if model.started then "Stop" else "Start") ]
                      ]
      , div floatLeft [ svgPendulum ]
      , div floatLeft [collage 500 700 [ pendulum ] |> fromElement]
      ]


control = Signal.mailbox NoOp

clicks = Mouse.position |> Signal.sampleOn Mouse.clicks |> Signal.map Click 

tickSignal =   (every (dt * second)) |> map (always Tick)

actionSignal = Signal.mergeMany [tickSignal, control.signal, clicks]

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

