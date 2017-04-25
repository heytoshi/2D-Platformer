--modified the old Elm Mario debugger from
--"http://debug.elm-lang.org/edit/Mario.elm"
-- The Cola and Himouto
-- Press A to run Left and B to run Right
-- Use the arrows keys to walk and Jump
-- updated better version of mario on the latest elm
-- need to implement Collision
--scoring
-- decrease score if character goes out of the playground

module Main exposing (..)

import AnimationFrame exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Html exposing (Html)
import Element exposing(..)
import Keyboard
import Task
import Time exposing (Time)
import Window




type alias Block =
    { color : Color }

size : Float
size = 25



tobForm : Block -> Form
tobForm block =
    let
        shape =
            square size

        border =
            outlined (solid Color.black) shape
    in
        group [ filled block.color shape, border ]



--Character Model
type alias Character =
                   { x : Float,
                    y : Float,
                    xv : Float,
                    vy : Float,
                    directory : Direction,
                    resolutionWindow : Window.Size,
                    keyClick : Keys}
--direction for the keys
type Direction
    = Left
    | Right
type alias Keys =  { x : Int, y : Int }

--model for the character
himouto : Character
himouto =
    { x = 0
    , y = 0
    , xv = 0
    , vy = 0
    , directory = Right
    , resolutionWindow = { width = 0, height = 0 }
    , keyClick = { x = 0, y = 0 }
    }

--Keyboard package
pressKey : Int -> Keyboard.KeyCode -> Keys -> Keys
pressKey scale key keyClick =
    case key of
      --CSS keyboard values
        65 -> { keyClick | x = scale - 3 }
        68 -> { keyClick | x = 3 }

        37 -> { keyClick | x = -scale }
        38 -> { keyClick | y = scale }
        39 -> { keyClick | x = scale }
        40 -> { keyClick | y = -scale }
        _ -> keyClick

--enables the key for the character
step : Msg -> Character -> ( Character, Cmd Msg )
step msg himouto = case msg of
        Frame dt ->
            ( himouto
            --enable the physics and actions on the character
                |> gravity (dt / 10)
                |> jump himouto.keyClick
                |> walk himouto.keyClick
                |> physics (dt / 10)
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



--jumping keys (control height of jump)
jump : Keys -> Character -> Character
jump keyClick himouto =
    if keyClick.y > 0 && himouto.vy == 0 then
        { himouto | vy = 5.0 }
    else
        himouto


gravity : Float -> Character -> Character
gravity dt himouto =
    { himouto
        | vy =  if himouto.y > 0 then
                himouto.vy - dt / 10
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
        directory = case himouto.directory of
                Left -> "left-him"
                Right -> "right-him"
        -- Custom Himouto gifs for the model of the character
        himoutoImgSource = "imgs/himouto/" ++ action ++ "/" ++ directory ++ ".gif"
        brick = "imgs/himouto/brick/brick.gif"
        background = "imgs/himouto/background/back.gif"
        block = "imgs/himouto/block/block.gif"
        cola = "imgs/himouto/cola/colar.png"
      --  backgroundImage = image 400 400 background
        himoutoImage =  image 35 28 himoutoImgSource
        colaImage = image 50 66 cola
        brickImage = image 399 181 brick
        blockImage = image 35 35 block
        groundY = 165 - height / 2
        -- Custom Brick background import
    in  collage
            himouto.resolutionWindow.width
            himouto.resolutionWindow.height
            [ rect 400 height
                |> filled (rgb 174 238 238)
            ,himoutoImage
                |> toForm
                |> move ( himouto.x, himouto.y + groundY )
            , brickImage
                |> toForm
                |> move (0, 63 - height / 2 )
            , blockImage
                |> toForm
                |> move (180, 200 - height / 2)
            , blockImage
                |> toForm
                |> move (145, 200 - height / 2)
            , blockImage
                |> toForm
                |> move (115, 200 - height / 2)
            , blockImage
                |> toForm
                |> move (80, 200 - height / 2)
            , blockImage
                |> toForm
                |> move (-180, 300 - height / 2)
            , blockImage
                |> toForm
                |> move (-145, 300 - height / 2)
            , blockImage
                |> toForm
                |> move (-115, 300 - height / 2)
            , blockImage
                |> toForm
                |> move (-80, 300 - height / 2)
            , blockImage
                |> toForm
                |> move (0, 400 - height / 2)
            , blockImage
                |> toForm
                |> move (80, 500 - height / 2)
            ,  blockImage
                |> toForm
                |> move (-180, 600 - height / 2)
            , blockImage
                |> toForm
                |> move (-145, 600 - height / 2)
            , blockImage
                |> toForm
                |> move (-115, 600 - height / 2)
            , blockImage
                |> toForm
                |> move (-80, 600 - height / 2)
            , colaImage
                |> toForm
                |> move (-145, 640 - height / 2)
            ]
            |> Element.toHtml



-- AnimationFrame package, Html package,
--makes the game run
type Msg
    = Frame Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | WindowSize Window.Size


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
