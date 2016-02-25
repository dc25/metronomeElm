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
import Svg.Attributes exposing (version, viewBox, x, y, fill,points, transform)
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
    pendulumEndpoint = ( 0, pendulumLength)
    weightPlacement = ( 0, (model.slideRatio) * pendulumLength)
    metronomeEndpoint = ( 0, -1.6 * pendulumLength )
    pendulum =
      group
        [ segment metronomeEndpoint pendulumEndpoint
            |> traced { defaultLine | width = 2, color = red }
        , circle 8
            |> filled blue
        , circle 12
            |> filled purple
            |> move weightPlacement
        , ngon 3 10
            |> filled green
            |> rotate (-pi/2)
            |> move metronomeEndpoint
        ]
      |> rotate (pi + model.angle) -- display "zero" angle is up but pendulum "zero" angle is down so rotate by pi to make them match.
  in
    div []
      [ div floatLeft [ fst model.click |> toString |> Html.text
                      , button 
                          [ onClick control.address ToggleStarted ]
                          [ Html.text (if model.started then "Stop" else "Start") ]
                      ]
      , div floatLeft [ logo ]
      , div floatLeft [collage 500 800 [ pendulum ] |> fromElement]
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


logo : Html
logo =
  svg [ version "1.1", x "0", y "0", Svg.Attributes.width "120", Svg.Attributes.height "120", viewBox "0 0 400 400" ]
    [ Svg.polygon [ fill "#F0AD00", points "161.649,152.782 231.514,82.916 91.783,82.916" ] []
    , Svg.polygon [ fill "#7FD13B", points "8.867,0 79.241,70.375 232.213,70.375 161.838,0" ] []
    , Svg.rect
        [ fill "#7FD13B", x "192.99", y "107.392", Svg.Attributes.width "107.676", Svg.Attributes.height "108.167"
        , transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
        ]
        []
    , Svg.polygon [ fill "#60B5CC", points "323.298,143.724 323.298,0 179.573,0" ] []
    , Svg.polygon [ fill "#5A6378", points "152.781,161.649 0,8.868 0,314.432" ] []
    , Svg.polygon [ fill "#F0AD00", points "255.522,246.655 323.298,314.432 323.298,178.879" ] []
    , Svg.polygon [ fill "#60B5CC", points "161.649,170.517 8.869,323.298 314.43,323.298" ] []
    ]

