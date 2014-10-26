import Debug
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
          speed   = 0.03,
          score   = Nothing }
     | not hold && started ->
        { started = False,
          meter   = meter,
          speed   = 0,
          score   = meter }
     | otherwise ->
        let newMeter = Maybe.map (\m -> clamp 0.0 1.0 <| m + speed) meter
            newSpeed = case newMeter of
                         Just m -> if m == 1.0 || m == 0.0 then -speed else speed
                         _      -> speed
        in { started = started,
             meter   = newMeter,
             speed   = newSpeed,
             score   = score }
  |> Debug.watch "state"
  
display : State -> Element
display { meter, score } =
  let background = fittedImage w h "background.jpg"
      cow        = image 525 400 "cow.png"
      barH       = 400
      meterVal   = case meter of
                     Just m -> m
                     _      -> 0
      meterY     = -barH/2 + barH * meterVal
  in collage w h [background |> toForm
                 , cow |> toForm |> move (-350, -30)
                 , rect 110 410  |> filled black  |> move (100, 0)
                 , rect 100 barH |> filled red    |> move (100, 0)
                 , rect 100 360  |> filled orange |> move (100, -20)
                 , rect 100 250  |> filled green  |> move (100, -75)
                 , ngon 3 40     |> filled black  |> move (10, meterY)
                 , ngon 3 30     |> filled white  |> move (10, meterY)]

hold  = sampleOn (fps 75) (Keyboard.isDown backtick)
state = foldp stepGame defaultState hold
main  = display <~ state