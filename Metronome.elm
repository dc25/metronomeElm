module Metronome where 

import Color exposing (..)
import Time exposing (..)
import Html exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)

dt = 0.01
scale = 100

type alias Model =
  { angle : Float
  , angVel : Float
  , length : Float
  , slideRatio : Float
  , gravity : Float
  }

init =
  { angle = 1 * pi / 4
  , angVel = 0.0
  , length = 2
  , slideRatio = 0.2
  , gravity = -9.81
  }

update : Model -> Model
update model =
  let
    angAcc = 1.0 * (model.gravity / (model.slideRatio * model.length)) * sin (model.angle)
    angVel' = model.angVel + angAcc * dt
    angle' = model.angle + angVel' * dt
  in
    { model
      | angle = angle'
      , angVel = angVel'
    }

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
  in
    collage 700 1000
      [ pendulum |> rotate (pi + model.angle) ]

modelSignal =  Signal.foldp (\_ model -> update model) init (every (dt * second))

leftRightSignal = modelSignal
                  |> Signal.map (\model -> model.angle < 0) 
                  |> Signal.dropRepeats 

port leftRight : Signal Bool
port leftRight = leftRightSignal

main = modelSignal |> Signal.map view


