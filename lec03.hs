{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.Text.Internal (Text)

-- Lists

data List a = Empty | Entry a (List a)
    
mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

elemList :: Eq a => a -> List a -> Bool
elemList _ Empty = False
elemList c' (Entry c cs)
  | c == c' = True
  | otherwise = elemList c' cs
-- Coordinates


data Coord = C Integer Integer deriving Eq

data Direction = R | U | L | D

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo = undefined


-- The maze

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
       
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c = case maze c of
  Box -> Ground
  t -> t

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty c' = noBoxMaze c'
mazeWithBoxes (Entry c cs) c'
  | c == c' = Box
  | otherwise = mazeWithBoxes cs c'

-- The state

data State = S Coord Direction (List Coord)


initialBoxes :: List Coord
initialBoxes = go (-10) (-10)
  where
    go 11 11 = Empty
    go  x 11 = go (x+1) (-10)
    go  x  y = case maze (C x y) of
      Box -> Entry (C x y) (go x (y+1))
      _   -> go x (y+1)

initialState :: State
initialState = S (C 0 1) R initialBoxes

-- Event handling

handleEvent :: Event -> State -> State
handleEvent (KeyPress "Right") s = handleMove s R
handleEvent (KeyPress "Up")    s = handleMove s U
handleEvent (KeyPress "Left")  s = handleMove s L
handleEvent (KeyPress "Down")  s = handleMove s D
handleEvent _ s = s

handleMove :: State -> Direction -> State
handleMove (S c _ bs) d = case nextTile of
  Ground  -> S next d bs
  Storage -> S next d bs
  Box     -> if nextAfterTile `elem` [Ground, Storage]
             then S next d (mapList (moveFromTo next nextAfter) bs)
             else S c d bs
  _       -> S c d bs

  where
    next = adjacentCoord d c
    nextTile = getTile next
    nextAfter = adjacentCoord d next
    nextAfterTile = getTile nextAfter
    getTile :: Coord -> Tile
    getTile c' = if c' `elemList` bs then Box else noBoxMaze c'
    moveFromTo :: Coord -> Coord -> (Coord -> Coord)
    moveFromTo src dst c = if src == c then dst else c

-- Drawing

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


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

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState (S c d cs) = atCoord c (player d) & (pictureOfBoxes cs) & pictureOfMaze

-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction initialState (\_ c -> c) handleEvent drawState

-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen
    
    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)
    
    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)
    
    draw' StartScreen = startScreen
    draw' (Running s) = draw s


-- The main function

main :: IO ()
main = runInteraction sokoban
