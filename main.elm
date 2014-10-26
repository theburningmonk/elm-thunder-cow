import Keyboard
import Maybe
import Signal
import Window

backtick = 223
w = 600
h = 500
type State   = { started:Bool, meter:Maybe Float, speed:Float, score:Maybe Float }
defaultState = { started=False, meter=Nothing, speed=0, score=Nothing }

stepGame : Bool -> State -> State
stepGame hold { started, meter, speed, score } =
  if | hold && not started ->
        { started = True,
          meter   = Just 0.0,
          speed   = 0.01,
          score   = Nothing }
     | not hold && started ->
        { started = False,
          meter   = Nothing,
          speed   = 0,
          score   = meter }
     | otherwise ->
        { started = started,
          meter   = Maybe.map (\m -> clamp 0.0 1.0 m) meter,
          speed   = case meter of
                      Just m -> if m == 1 || m == 0 then -speed else speed
                      _      -> speed,
          score   = score }
  
display : State -> Element
display state =
  let background = fittedImage w h "background.jpg"
      cow = image 525 400 "cow.png"
  in collage w h [background |> toForm
                 , cow |> toForm |> move (-350, -30)
                 , rect 110 410 |> filled black |> move (100, 0)
                 , rect 100 400 |> filled red |> move (100, 0)
                 , rect 100 360 |> filled orange |> move (100, -20)
                 , rect 100 250 |> filled green |> move (100, -75)]

hold  = sampleOn (fps 60) (Keyboard.isDown backtick)
state = foldp stepGame defaultState hold
main  = display <~ state