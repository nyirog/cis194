{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.Text.Internal

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box =     colored brown      (solidRectangle 1 1)

player :: Direction -> Picture
player R = translated 0 0.3 cranium
          & path [(0,0),(0.3,0.05)] 
          & path [(0,0),(0.3,-0.05)] 
          & path [(0,-0.2),(0,0.1)] 
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
          & path [(0,0),(0.3,0.05)] 
          & path [(0,0),(-0.3,0.05)] 
          & path [(0,-0.2),(0,0.1)] 
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
          & path [(0,0),(0.3,-0.05)] 
          & path [(0,0),(-0.3,-0.05)] 
          & path [(0,-0.2),(0,0.1)] 
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)


data Tile = Wall | Ground | Storage | Box | Blank | Player deriving Eq

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt r c))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze (C r c)))
         
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

data Direction = R | U | L | D

data Coord = C Integer Integer

data State = S Coord Direction


initialState :: State
initialState = S (C 0 1) R

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Coord -> Direction -> Coord
adjacentCoord (C x y) R = C (x+1) y
adjacentCoord (C x y) U = C  x   (y+1)
adjacentCoord (C x y) L = C (x-1) y
adjacentCoord (C x y) D = C  x   (y-1)

handleTime :: Double -> State -> State
handleTime _ c = c

handleEvent :: Event -> State -> State
handleEvent (KeyPress "Esc")   _       = initialState
handleEvent (KeyPress "Right") (S c _) = S (nextCoord c R) R
handleEvent (KeyPress "Up")    (S c _) = S (nextCoord c U) U
handleEvent (KeyPress "Left")  (S c _) = S (nextCoord c L) L
handleEvent (KeyPress "Down")  (S c _) = S (nextCoord c D) D
handleEvent _                  s       = s

nextCoord :: Coord -> Direction -> Coord
nextCoord c d 
  = let next = adjacentCoord c d
    in if (maze next) `elem` [Ground, Storage] then next else c


drawState :: State -> Picture
drawState (S c d) = atCoord c (player d) & pictureOfMaze

main :: IO ()
main = interactionOf initialState handleTime handleEvent drawState
