--modified the old Elm Mario debugger from
--"http://debug.elm-lang.org/edit/Mario.elm"
-- The Cola and Himouto
-- Use the arrows keys to walk and Jump
-- monster gif by Michael Masson on pixelart

module Main exposing (..)

import AnimationFrame exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Html exposing (Html)
import Element exposing(..)
import Keyboard exposing (..)
import Task
import Text
import Time exposing (..)
import Window exposing (Size)
import Collision2D
import Mouse
import Char
import Set exposing (Set)
import Collision exposing (..)

--Character Model
type alias Character =
                   { x : Float,
                    y : Float,
                    xv : Float,
                    vy : Float,
                    score : Int,
                    dead : Bool,
                    monster : Float,
                    door : Float,
                    alive : Bool,
                    speed2 : Float,
                    door3 : Float,
                    alive3 : Bool,
                    door2 : Float,
                    alive2 : Bool,
                    skeleton2 : Float,
                    skelBool2 : Bool,
                    skeleton : Float,
                    skelBool : Bool,
                    cola : Float,
                    colaBool : Bool,
                    extraScore2 : Int,
                    extraScore : Int,
                    state : State,
                    speed : Float,
                    directory : Direction,
                    resolutionWindow : Window.Size,
                    keyClick : Keys,
                    keyClickPlayer2 : Keys,
                    keysDown : Set KeyCode
                    }

--direction for the keys
type Direction
    = Left
    | Right

type alias Keys =  { x : Int,
                     y : Int }

type State = Play | Pause


--model for the character
himouto : Character
himouto =
    { x = 0
    , y = 0
    , xv = 0
    , vy = 0
    , score = 0
    , dead = False
    , state = Pause
    , monster = 350
    , directory = Right
    , speed = 3
    , door = 350
    , alive = False
    , door2 = 350
    , alive2 = False
    , door3 = 350
    , alive3 = False
    , cola = 350
    , extraScore = 0
    , colaBool = False
    , skeleton = 350
    , skelBool = False
    , skeleton2 = 350
    , skelBool2 = False
    , extraScore2 = 0
    , speed2 = 3
    , resolutionWindow = { width = 0, height = 0 }
    , keyClick = { x = 0, y = 0 }
    , keyClickPlayer2  = { x = 0, y = 0 }
    , keysDown = Set.empty
    }

--Keyboard package
pressKey : Int -> Keyboard.KeyCode -> Keys -> Keys
pressKey scale key keyClick =
    case key of
      --CSS keyboard values
        37 -> { keyClick | x = -scale } --left arrows
        38 -> { keyClick | y = scale } --up arrows
        39 -> { keyClick | x = scale } --right arrows
        40 -> { keyClick | y = -scale } --down arrows
        _ -> keyClick


--enables the key for the character
step : Msg -> Character -> ( Character, Cmd Msg )
step msg himouto = case msg of
        Frame dt ->
            ( himouto
            --enable the physics and actions on the character
            --enable Collision
                |> gravity (dt / 10)
                |> jump himouto.keyClick
                |> walk himouto.keyClick
                |> physics (dt / 10)
                |> death dt
                |> monster dt
                |> death2 dt
                |> door dt
                |> teleport dt
                |> door2 dt
                |> teleport2 dt
                |> door3 dt
                |> teleport3 dt
                |> cola dt
                |> cocacola dt
                |> skeleton dt
                |> skeleton2 dt
                |> skeletonDeath dt
                |> skeletonDeath2 dt

            , Cmd.none )
            --make sure the character stops after clicking once
        KeyDown key ->
            ( { himouto | keyClick = pressKey 1 key himouto.keyClick  }
            , Cmd.none)
        KeyUp key ->
            ( { himouto | keyClick = pressKey 0 key himouto.keyClick }
            , Cmd.none )
        WindowSize size ->
            ( { himouto | resolutionWindow = size }
            , Cmd.none )

monster : Float -> Character -> Character
monster dt himouto =
  let
    newRound = himouto.monster < -100
  in
  { himouto |
      monster =
      if himouto.dead then himouto.monster
      else
        if newRound then 100
        else himouto.monster - himouto.speed,
        speed = if newRound then himouto.speed + 0.00001 else himouto.speed
  }



door : Float -> Character -> Character
door dt himouto =
    let
      newRound = himouto.door < 150
    in
    { himouto |
        door =
        if himouto.alive then himouto.door
        else
          if newRound then -150
          else himouto.door - himouto.speed2,
          speed2 = if newRound then himouto.speed2 + 0.01 else himouto.speed2
   }
--lets u teleport the a another floor
teleport : Float -> Character -> Character
teleport dt himouto =
  let
    vDistance = himouto.y - 35
    hDistance = abs ((himouto.x - 15) - himouto.door)
    maybeAlive = himouto.alive || (hDistance < 5 && vDistance < 5)
  in
  { himouto |
      alive = maybeAlive
  }

door2 : Float -> Character -> Character
door2 dt himouto =
    let
      newRound = himouto.door2 < 165
    in
    { himouto |
        door2 =
        if himouto.alive2 then himouto.door2
        else
          if newRound then 155
          else himouto.door2 - himouto.speed2,
          speed2 = if newRound then himouto.speed2 else himouto.speed2
   }
--lets u teleport the a another floor
teleport2 : Float -> Character -> Character
teleport2 dt himouto =
  let
    vDistance = himouto.y - 35
    hDistance = abs ((himouto.x + 20) - himouto.door2)
    maybeAlive2 = himouto.alive2 || (hDistance < 5 && vDistance < 5)
  in
  { himouto |
      alive2 = maybeAlive2
  }
-- not a door but a dragon
door3 : Float -> Character -> Character
door3 dt himouto =
    let
      newRound = himouto.door3 < 370
    in
    { himouto |
        door3 =
        if himouto.alive3 then himouto.door3
        else
          if newRound then -370
          else himouto.door3 - himouto.speed2,
          speed2 = if newRound then himouto.speed2 else himouto.speed2
   }

--lets u teleport the a another floor
teleport3 : Float -> Character -> Character
teleport3 dt himouto =
  let
    vDistance = himouto.y - 65
    hDistance = abs ((himouto.x - 40) - himouto.door3)
    maybeAlive3 = himouto.alive3 || (hDistance < 5 && vDistance < 5)
  in
  { himouto |
      alive3 = maybeAlive3
  }

cola : Float -> Character -> Character
cola dt himouto =
    let
      newRound = himouto.cola < 300
    in
    { himouto |
        cola =
        if himouto.colaBool then himouto.cola
        else
          if newRound then -300
          else himouto.door - himouto.speed2,
          speed2 = if newRound then himouto.speed2 else himouto.speed2
   }

--Cola is instant win
cocacola : Float -> Character -> Character
cocacola dt himouto =
  let
    vDistance = himouto.y - 35
    hDistance = abs ((himouto.x - 20) - himouto.cola)
    maybeCola = himouto.colaBool || (hDistance < 5 && vDistance < 5)
  in
  { himouto |
      colaBool = maybeCola
  }

death : Float -> Character -> Character
-- dead if himouto aproaches from RHS
death dt himouto =
  let
    vDistance = himouto.y - 35
    hDistance = abs ((himouto.x - 20) - himouto.monster)
    maybeDead = himouto.dead || (hDistance < 5 && vDistance < 5)
  in
  { himouto |
      dead = maybeDead,
      score = if maybeDead then himouto.score else himouto.score + 1
  }



skeleton : Float -> Character -> Character
skeleton dt himouto =
    let
      newRound = himouto.skeleton < 200
    in
    { himouto |
        skeleton =
        if himouto.skelBool then himouto.skeleton
        else
          if newRound then -200
          else himouto.skeleton - himouto.speed2,
          speed2 = if newRound then himouto.speed2 else himouto.speed2
   }

skeletonDeath : Float -> Character -> Character
skeletonDeath dt himouto =
  let
    vDistance = himouto.y - 35
    hDistance = abs ((himouto.x - 20) - himouto.skeleton)
    maybeSkel = himouto.skelBool || (hDistance < 5 && vDistance < 5)
  in
  { himouto |
      skelBool = maybeSkel,
      extraScore = if maybeSkel then himouto.extraScore else himouto.extraScore + 1
  }

skeleton2 : Float -> Character -> Character
skeleton2 dt himouto =
    let
      newRound = himouto.skeleton2 < 200
    in
    { himouto |
        skeleton2 =
        if himouto.skelBool2 then himouto.skeleton2
        else
          if newRound then 199
          else himouto.skeleton2 - himouto.speed
   }

skeletonDeath2 : Float -> Character -> Character
skeletonDeath2 dt himouto =
  let
    vDistance = himouto.y - 25
    hDistance = abs ((himouto.x + 20) - himouto.skeleton2)
    maybeSkel = himouto.skelBool2 || (hDistance < 5 && vDistance < 5)
  in
  { himouto |
      skelBool2 = maybeSkel,
      extraScore2 = if maybeSkel then himouto.extraScore2 else himouto.extraScore2 + 1
  }

-- dead if himouto aproaches from LHS
death2 : Float -> Character -> Character
death2 dt himouto =
  let
    vDistance = himouto.y - 35
    hDistance = abs ((himouto.x + 20) - himouto.monster)
    maybeDead = himouto.dead || (hDistance < 5 && vDistance < 5)
  in
  { himouto |
      dead = maybeDead,
      score = if maybeDead then himouto.score else himouto.score + 1
  }
--jumping keys (control height of jump)
jump : Keys -> Character -> Character
jump keyClick himouto =
    if keyClick.y > 0 && himouto.vy == 0 then
        { himouto | vy = 6.3 }
    else
        himouto


gravity : Float -> Character -> Character
gravity dt himouto =
    { himouto
        | vy =  if himouto.y > 0 then
                himouto.vy - dt / 3
            else
                0
    }


physics : Float -> Character -> Character
physics dt himouto =
    { himouto
        | x = himouto.x + dt * himouto.xv
        , y = max 0 (himouto.y + dt * himouto.vy)
    }


walk : Keys -> Character -> Character
walk keyClick himouto =
    { himouto
        | xv = toFloat keyClick.x
        , directory =
            if keyClick.x < 0 then
                Left
            else if keyClick.x > 0 then
                Right
            else
                himouto.directory }


whiteText =
  rgb 255 255 255

txt f string =
  Text.fromString string
    |> Text.color whiteText
    |> Text.monospace
    |> f
    |> leftAligned

-- Collage package, Html package, Element package, window package
show : Character -> Html Msg
show himouto =
    let
        ( width, height ) =
            ( toFloat himouto.resolutionWindow.width
            , toFloat himouto.resolutionWindow.height)

        action =
            if himouto.y > 0 then
                "jump"
            else if himouto.xv /= 0 then
                "walk"
            else
                "stand"

        scores = txt (Text.height 50) ("Time Spent: " ++ toString himouto.score )
        extraScores = txt (Text.height 50) ("Time Spent: " ++ toString himouto.extraScore )
        extraScores2 = txt (Text.height 50) ("Time Spent: " ++ toString himouto.extraScore2 )
        restart = txt (Text.height 50) ("Press F5 to restart the game ")

        directory = case himouto.directory of
                Left -> "left-him"
                Right -> "right-him"
        -- Custom Himouto gifs for the model of the character
        himoutoImgSource = "imgs/himouto/" ++ action ++ "/" ++ directory ++ ".gif"
        brick = "imgs/himouto/brick/brick.gif"
        easter = "imgs/himouto/easterEgg/egg.gif"
        background = "imgs/himouto/background/back.jpg"
        block = "imgs/himouto/block/block.gif"
        cola = "imgs/himouto/cola/colar.png"
        gover = "imgs/himouto/gameover/over.jpg"
        monster = "imgs/himouto/monster/monster2.gif"
        sonic = "imgs/himouto/sonic/sonic.gif"
        sonicImage = image 35 35 sonic
        door = "imgs/himouto/pit/pit.gif"
        skeleton = "imgs/himouto/skeleton/skeleton.gif"
        skel = "imgs/himouto/skel/skel.gif"
        fly = "imgs/himouto/fly/fly.gif"
        win = "imgs/himouto/win/win.png"
        dragon = "imgs/himouto/dragon/dragon.gif"
        himoutoCola = "imgs/himouto/himoutoCola/cola.gif"

        winCola = image 466 306 himoutoCola
        eggImage =  image 466 305 easter
        dragonImage = image 150 150 dragon
        winImage = image 300 200 win
        flyImage = image 35 35 fly
        skelImage = image 35 43 skel
        skeletonImage = image 40 40 skeleton
        doorImage = image 32 48 door
        backgroundImage = image 1920 1080 background
        monsterIMG = image 35 35 monster
        over = image 300 200 gover
        himoutoImage =  image 35 28 himoutoImgSource
        colaImage = image 50 66 cola
        brickImage = image 400 181 brick
        blockImage = image 35 35 block
        groundY = 165 - height / 2
        -- Custom Brick background import
    in  collage
            himouto.resolutionWindow.width
            himouto.resolutionWindow.height
            [ backgroundImage
                |> toForm
                |> move (0, height / 6.5 )
            , skeletonImage  |> toForm  |> move (himouto.skeleton, groundY + 10)
            , skeletonImage  |> toForm  |> move (himouto.skeleton, groundY + 50)
            , skeletonImage  |> toForm  |> move (himouto.skeleton, groundY + 90)
            , skeletonImage  |> toForm  |> move (himouto.skeleton, groundY + 180)
            , skeletonImage  |> toForm  |> move (himouto.skeleton, groundY + 220)
            , skeletonImage  |> toForm  |> move (himouto.skeleton, groundY + 340)
            , skeletonImage  |> toForm  |> move (himouto.skeleton, groundY + 260)
            , skeletonImage  |> toForm  |> move (himouto.skeleton2, groundY + 10)
            , skeletonImage  |> toForm  |> move (himouto.skeleton2, groundY + 10)
            , skeletonImage  |> toForm  |> move (himouto.skeleton2, groundY + 50)
            , skeletonImage  |> toForm  |> move (himouto.skeleton2, groundY + 90)
            , skeletonImage  |> toForm  |> move (himouto.skeleton2, groundY + 180)
            , skeletonImage  |> toForm  |> move (himouto.skeleton2, groundY + 220)
            , skeletonImage  |> toForm  |> move (himouto.skeleton2, groundY + 260)
            , skeletonImage  |> toForm  |> move (himouto.skeleton2, groundY + 340)
            , skeletonImage  |> toForm  |> move (himouto.skeleton2, groundY + 380)
            , skeletonImage  |> toForm  |> move (himouto.skeleton2, groundY + 420)
            , doorImage  |> toForm  |> move (himouto.door, 340 - height / 2)
            , doorImage  |> toForm  |> move (himouto.door, 175 - height / 2)
            , doorImage  |> toForm  |> move (himouto.door2, 340 - height / 2)
            , doorImage  |> toForm  |> move (himouto.door2, 500 - height / 2)
            , dragonImage |> toForm |> move (himouto.door3, 550 - height / 2)
            , himoutoImage
                |> toForm
                |> alpha (if himouto.dead then 0 else if himouto.alive then 0 else if himouto.alive2 then 0 else if himouto.skelBool then 0 else if himouto.skelBool2 then 0 else 1 )
                |> move ( himouto.x, himouto.y + groundY )
                |> rotate (if himouto.dead then 90 else 0)
            , himoutoImage
                |> toForm
                |> move (if himouto.alive then (himouto.x, himouto.y + 330 - height / 2) else (-9999, 0) )
                |> rotate (if himouto.dead then 90 else 0)
                |> alpha (if himouto.dead then 0 else if himouto.alive2 then 0 else if himouto.skelBool then 0 else if himouto.skelBool2 then 0 else 1 )
            , himoutoImage
                |> toForm
                |> move (if himouto.alive2 then (himouto.x, himouto.y + 490 - height / 2) else (-9999, 0) )
                |> rotate (if himouto.dead then 90 else 0)
                |> alpha (if himouto.dead then 0 else if himouto.alive2 then 1 else if himouto.skelBool then 0 else if himouto.skelBool2 then 0 else 1 )
            , himoutoImage
                |> toForm
                |> alpha (if himouto.dead then 0 else if himouto.alive then 0 else if himouto.alive2 then 0 else if himouto.skelBool then 0 else if himouto.skelBool2 then 0 else 1 )
                |> move ( himouto.x, himouto.y + groundY )
                |> rotate (if himouto.dead then 90 else 0)
            , monsterIMG
                |> toForm
                |> move (himouto.monster, groundY)
                |> alpha (if himouto.alive then 0 else 1)
            , skelImage
                |> toForm
                |> move (if himouto.alive then (himouto.monster, 340 - height / 2) else (-9999, 0) )
                |> alpha (if himouto.alive then 1 else 0)
                |> alpha (if himouto.alive2 then 0 else 1)
            , flyImage
                |> toForm
                |> move (if himouto.alive2 then (himouto.monster, 490 - height / 2) else (-9999, 0) )
            , colaImage
                |> toForm
                |> move (himouto.cola, 500 - height /2)
            , brickImage  |> toForm |> move (800, 63 - height / 2)
            , brickImage  |> toForm |> move (400, 63 - height / 2)
            , brickImage  |> toForm |> move (0, 63 - height / 2)
            , brickImage  |> toForm |> move (-400, 63 - height / 2)
            , brickImage  |> toForm |> move (-800, 63 - height / 2)
            , blockImage  |> toForm |> move (180, 300 - height / 2)
            , blockImage  |> toForm |> move (145, 300 - height / 2)
            , blockImage  |> toForm |> move (115, 300 - height / 2)
            , blockImage  |> toForm |> move (80, 300 - height / 2)
            , blockImage  |> toForm |> move (-180, 300 - height / 2)
            , blockImage  |> toForm |> move (-145, 300 - height / 2)
            , blockImage  |> toForm |> move (-115, 300 - height / 2)
            , blockImage  |> toForm |> move (-80, 300 - height / 2)
            , blockImage  |> toForm |> move (10, 300 - height / 2)
            , blockImage  |> toForm |> move (45, 300 - height / 2)
            , blockImage  |> toForm |> move (45, 300 - height / 2)
            , blockImage  |> toForm |> move (-15, 300 - height / 2)
            , blockImage  |> toForm |> move (-45, 300 - height / 2)
            , blockImage  |> toForm |> move (180, 460 - height / 2)
            , blockImage  |> toForm |> move (145, 460 - height / 2)
            , blockImage  |> toForm |> move (115, 460 - height / 2)
            , blockImage  |> toForm |> move (80, 460 - height / 2)
            , blockImage  |> toForm |> move (-180, 460 - height / 2)
            , blockImage  |> toForm |> move (-145, 460 - height / 2)
            , blockImage  |> toForm |> move (-115, 460 - height / 2)
            , blockImage  |> toForm |> move (-80, 460 - height / 2)
            , blockImage  |> toForm |> move (10, 460 - height / 2)
            , blockImage  |> toForm |> move (45, 460 - height / 2)
            , blockImage  |> toForm |> move (45, 460 - height / 2)
            , blockImage  |> toForm |> move (-15, 460 - height / 2)
            , blockImage  |> toForm |> move (-45, 460 - height / 2)
            , blockImage  |> toForm |> move (-245, 460 - height / 2)
            , blockImage  |> toForm |> move (-215, 460 - height / 2)
            , blockImage  |> toForm |> move (-275, 460 - height / 2)
            , blockImage  |> toForm |> move (-310, 460 - height / 2)
            , rect width height
                |> filled (rgb 0 0 0)
                |> move (if himouto.dead then (0, 0) else if himouto.skelBool then (0,0) else if himouto.colaBool then (0,0) else if himouto.skelBool2 then (0,0)  else if himouto.alive3 then (0,0)  else (-9999, 0))
            , over |> toForm |> move (if himouto.dead then (0, 0) else (-9999, 0))
            , over |> toForm |> move (if himouto.skelBool2 then (0, 0) else (-9999, 0))
            , over |> toForm |> move (if himouto.skelBool then (0, 0) else (-9999, 0))
            , eggImage |> toForm |> move (if himouto.alive3 then (0, 0) else (-9999, 0))
            , toForm scores
                |> move (if himouto.dead then (0, height / 2 - 30) else (-9999, 0))
                |> alpha (if himouto.skelBool2 then 0 else if himouto.skelBool then 0 else 1)
            , toForm extraScores
                |> move (if himouto.skelBool then (0, height / 2 - 30) else (-9999, 0))
            , toForm extraScores2
                |> move (if himouto.skelBool2 then (0, height / 2 - 30) else (-9999, 0))
            , toForm restart
                |> move (if himouto.dead then (0, height / 2 - 70) else (-9999, 0))
                |> alpha (if himouto.skelBool2 then 0 else if himouto.skelBool then 0 else 1)
            , toForm restart
                |> move (if himouto.skelBool then (0, height / 2 - 70) else (-9999, 0))
            , toForm restart
                |> move (if himouto.skelBool2 then (0, height / 2 - 70) else (-9999, 0))
            , winCola
                |> toForm
                |> move (if himouto.colaBool then (0, 0) else (-9999, 0))

            ]
                |> Element.toHtml

type Msg
    = Frame Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | WindowSize Window.Size

--AnimationFrame package, Html package,
--makes the game run
main : Program Never Character Msg
main =
    Html.program
        { init = ( himouto, Window.size |> Task.perform WindowSize )
        , update = step
        , view = show
        , subscriptions =
            \himouto ->
                Sub.batch
                    [ Keyboard.downs KeyDown
                    , Keyboard.ups KeyUp
                    , AnimationFrame.diffs Frame
                    , Window.resizes WindowSize
                    ]
        }
