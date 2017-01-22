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

pictureOfMaze :: (Coord -> Tile) -> Picture
pictureOfMaze tile = draw21times (\r -> draw21times (\c -> drawTileAt tile (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: (Coord -> Tile) -> Coord ->Picture
drawTileAt tile coord@(C row col) = translated (fromIntegral row) (fromIntegral col) (drawTile (tile coord))
         
maze1 :: Coord -> Tile 
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
         
maze2 :: Coord -> Tile 
maze2 (C x y)
  | abs x > 4  || abs y > 4     = Blank
  | abs x == 4 || abs y == 4    = Wall
  | x == -1 && y <= -2          = Wall
  | x ==  1 && y <= -2          = Wall
  | x ==  0 && y <= -2          = Storage
  | x >= -2 && x <= 2 && y == 0 = Box
  | otherwise                   = Ground

data Direction = R | U | L | D | O

data Coord = C Integer Integer

data Level = First | Second

data State = S Coord Direction Level

selectMaze :: Level -> (Coord -> Tile)
selectMaze First  = maze1
selectMaze Second = maze2

initialState :: State
initialState = S (C 0 1) R First

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Coord -> Direction -> Coord
adjacentCoord (C x y) R = C (x+1) y
adjacentCoord (C x y) U = C  x   (y+1)
adjacentCoord (C x y) L = C (x-1) y
adjacentCoord (C x y) D = C  x   (y-1)
adjacentCoord c       O = c

handleTime :: Double -> State -> State
handleTime _ s = s

handleEvent :: Event -> State -> State
handleEvent (KeyPress "Esc") _ = initialState
handleEvent (KeyPress "1")   _ = S (C 0 1) R First
handleEvent (KeyPress "2")   _ = S (C 0 1) R Second

handleEvent (KeyPress key) (S c _ l)
  = let d = dir key
        s = S c d l
    in S (nextCoord s) d l
  where
    dir :: Text -> Direction
    dir "Right"  = R
    dir "Up"     = U
    dir "Left"   = L
    dir "Down"   = D
    dir _        = O

    nextCoord :: State -> Coord
    nextCoord (S c d l)
      = let next = adjacentCoord c d
            maze = selectMaze l
            tile = maze next
        in if tile `elem` [Ground, Storage] then next else c

handleEvent _                  s       = s

drawState :: State -> Picture
drawState (S c d l) = atCoord c (player d) & (pictureOfMaze (selectMaze l))

main :: IO ()
main = interactionOf initialState handleTime handleEvent drawState
