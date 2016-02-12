import Time exposing (..)
import Html exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)

type alias Model = Int

update : Time -> Model -> Model
update _ model = model + 1

-- view : Model -> Html
view m = show m



-- main : Signal Html
main =    Signal.foldp update 0 (every second)
       |> Signal.map view 

