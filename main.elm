import Debug
import Keyboard
import Maybe
import Signal
import Window

backtick = 223
w        = 600
h        = 500
barH     = 400
great    = 0.1 -- get into top 10%
notBad   = 0.4 -- get into top 40%
type RoundState  = { meter:Float, speed:Float }
data ResultKind  = Great | NotBad | Ok
type ResultState = { score:Float, kind:ResultKind, time:Int }
data GameState   = Round  RoundState
                   | Result ResultState
                   | NotStarted

stepGame : Bool -> GameState -> GameState
stepGame hold state =
  case state of
    NotStarted -> if hold then Round { meter=0.0, speed=0.025 } else state
    Round {meter, speed} -> 
      if not hold 
      then 
        let kind = if | meter >= 1 - great  -> Great
                      | meter >= 1 - notBad -> NotBad
                      | otherwise           -> Ok
        in Result { score=meter, kind=kind, time=180 }
      else
        let newMeter = clamp 0 1 <| meter + speed
            newSpeed = if newMeter == 1 || newMeter == 0 then -speed else speed
        in Round { meter=newMeter, speed=newSpeed }
    Result restulState ->
      if restulState.time == 0 then NotStarted
      else Result { restulState | time <- restulState.time-1 }
  
drawBackground () =
  let background = fittedImage w h "background.jpg"
      cow        = image 525 400 "cow.png"
  in collage w h [background |> toForm
                 , cow |> toForm |> move (-350, -30)
                 , rect 110 410  |> filled black  |> move (200, 0)
                 , rect 100 barH |> filled red    |> move (200, 0)
                 , rect 100 (barH * (1 - great))  |> filled orange |> move (200, -barH * great/2)
                 , rect 100 (barH * (1 - notBad)) |> filled green  |> move (200, -barH * notBad/2)]

drawArrow state = 
  let meterVal   = case state of
                     Round { meter }  -> meter
                     Result { score } -> score
                     _      -> 0
      meterY     = -barH/2 + barH * meterVal
  in collage w h [ngon 3 40     |> filled black  |> move (110, meterY)
                , ngon 3 30     |> filled white  |> move (110, meterY)]

drawResult state =
  case state of
    Result { kind } ->
      let img = case kind of 
                  Great  -> "red_fart.png"
                  NotBad -> "orange_fart.png"
                  _      -> "green_fart.png"                
      in collage w h [img |> image 200 125 |> toForm |> move (-30, 70)]
    _ -> collage w h []

display : GameState -> Element
display state =
  layers [ drawBackground(), drawArrow state, drawResult state ]

hold  = sampleOn (fps 75) (Keyboard.isDown backtick)
state = foldp stepGame NotStarted hold
main  = display <~ state