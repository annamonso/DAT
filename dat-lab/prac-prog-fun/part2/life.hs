
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw
import Drawing.Vector
import Data.Text (Text, pack)

import Drawing

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board,      -- last board generation
          gmGridMode :: GridMode,
          gmZoom :: Double, 
          gmShift :: Point,
          gmPaused :: Bool, 
          gmInterval :: Time, --generation interbal when not
          gmElapsedTime :: Time --elapsed time from last
        }

setGmBoard x g       = g{ gmBoard = x }
setGmGridMode x g    = g{ gmGridMode = x }
setGmZoom x g        = g{ gmZoom = x }
setGmShift x g       = g{ gmShift = x }
setGmPaused x g      = g{ gmPaused = x }
setGmInterval x g    = g{ gmInterval = x }
setGmElapsedTime x g = g{ gmElapsedTime = x }

data GridMode = NoGrid | LivesGrid | ViewGrid
-----------------------------------------------------
-- Initialization

viewWidth, viewHeight :: Double
viewWidth = 60.0
viewHeight = 30.0

main :: IO ()
main =
    activityOf viewWidth viewHeight initial handleEvent draw

board0Cells =
    [(-5, 0), (-4, 0), (-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]

initial = Game
    { gmBoard = foldr (setCell True) initBoard board0Cells
    , gmGridMode = NoGrid,
    gmZoom = 1.0,
    gmShift = (0.0 , 0.0),
    gmPaused = True,
    gmInterval = 1.0,
    gmElapsedTime = 0.0
    
    }

-----------------------------------------------------
-- Functions

nextGridMode :: GridMode -> GridMode
nextGridMode NoGrid = LivesGrid
nextGridMode LivesGrid = ViewGrid
nextGridMode ViewGrid = NoGrid

nextPausedMode :: Bool -> Bool
nextPausedMode True = False
nextPausedMode False = True

pointToPos :: Point -> Game -> Pos
pointToPos p game =
    let 
    ( gx , gy ) = ( 1.0 / gmZoom game ) *^ p ^-^ gmShift game
    in 
    (round gx , round gy )
    
info :: [(String,String)]
info =[ ("N","NextStep"), ("G","Change grid mode"), ("O","Zoom out"), ("I","Zoom in"), ("ARROWRIGHT","Shift left"), ("ARROWLEFT","Shift right"), ("SPACE","Pause/run toggle"), ("+","Increase velocity"),("-","Decrease Velocity"),("Use the mouse to set live/dead cells","") ]

realinfo :: Game -> [(String,String)]
realinfo g = [(show(1.0/gmInterval g) ++ "steps per second" ++ (if (gmPaused g) then " paused" else ""),""),("Zoom = " ++ show(gmZoom g)," "), ( "Shift = " ++ show(fst(gmShift g))++ ","++ show(snd(gmShift g)) , "")]
  
  
drawLines ls = foldMap drawLine(zip [0..] ls)

drawLine :: (Int, (String,String)) -> TextAnchor -> Double -> Drawing
drawLine (i,(s1,s2)) t j = 
    let
        color = if j == 1 then red else blue
        dx1 = ((j*viewWidth)/ 2 - j)
        dx2 = dx1 +5.5
        dy = (viewHeight/ 2 - 1 - fromIntegral i)
    in colored color $ translated dx1 dy (atext t (pack s1)) <> translated dx2 dy (atext t (pack s2))


-----------------------------------------------------
-- Event processing

handleEvent :: Event -> Game -> Game

handleEvent (KeyDown "N") game =                -- Next generation
    setGmBoard (nextGeneration (gmBoard game)) game
    
handleEvent (KeyDown "G") game =                -- Next generation
    setGmGridMode (nextGridMode(gmGridMode game)) game

handleEvent (MouseDown (x, y)) game =           -- Set live/dead cells
    let pos = pointToPos (x,y) game
        brd = gmBoard game
    in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game
    
    
handleEvent (KeyDown "I") game = --Zoom in
    if gmZoom game < 64.0 then setGmZoom (gmZoom game * 2.0 ) game
    else game
    
handleEvent (KeyDown "O") game = --Zoom out
    if gmZoom game > (0.03125) then setGmZoom (gmZoom game / (2.0) ) game
    else game
    
handleEvent (KeyDown "ARROWUP" ) game = --Down shift
    setGmShift ( gmShift game ^-^ ( 1.0 / gmZoom game ) *^ ( 0 , 5 ) ) game
    
handleEvent (KeyDown "ARROWDOWN" ) game = --Up shift
    setGmShift ( gmShift game ^-^ ( 1.0 / gmZoom game ) *^ ( 0 , -5 ) ) game
    
handleEvent (KeyDown "ARROWRIGHT" ) game = --left shift
    setGmShift ( gmShift game ^-^ ( 1.0 / gmZoom game ) *^ ( 5 , 0 ) ) game

handleEvent (KeyDown "ARROWLEFT" ) game = --right shift
    setGmShift ( gmShift game ^-^ ( 1.0 / gmZoom game ) *^ ( -5 , 0 ) ) game

handleEvent (KeyDown " " ) game = --automatica
    setGmPaused (nextPausedMode(gmPaused game)) game   

handleEvent (KeyDown "+" ) game = --aumenta la velocitat
    setGmInterval ( gmInterval game * (0.5) ) game

handleEvent (KeyDown "-" ) game = --disminueix la velocitat
    setGmInterval ( gmInterval game * (2) ) game
   

    
     
handleEvent (TimePassing dt) game = -- produit amb el pas del temps
    let
        time = gmElapsedTime game + dt
    in 
        if time >= gmInterval game && not (gmPaused game) 
        then do 
            setGmElapsedTime 0 $ setGmBoard (nextGeneration (gmBoard game)) game
             
        else setGmElapsedTime time game

handleEvent _ game =
    game

-----------------------------------------------------
-- Drawing


draw game =
    let
        board = gmBoard game
        grid = gmGridMode game
       
        zoom x = scaled (gmZoom game) (gmZoom game) x
        
        move x = translated (fst(gmShift game)) (snd (gmShift game)) x
        
        x = viewWidth/2
        y = viewHeight/2
        
        lines = if gmPaused game
                then 
                    drawLines info startAnchor (-1)  <> drawLines (realinfo game) endAnchor (1)  
                else 
                    drawLines (realinfo game) endAnchor (1)
    in 
       case grid of
        NoGrid -> zoom ( move ( drawBoard (gmBoard game))) <> lines
        LivesGrid -> zoom ( move ( drawBoard (gmBoard game))) <> zoom ( move ( drawGrid (minLiveCell board) (maxLiveCell board))) <> lines 
        ViewGrid ->  zoom ( move ( drawBoard (gmBoard game) <> drawGrid (pointToPos ((-x), (-y)) game ) (pointToPos (x, y) game ))) <>lines
        
        
        
        

