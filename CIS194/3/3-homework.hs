{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists
data List a = Empty | Entry a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

-- Coordinates

data Coord = C Integer Integer

data Direction = R | U | L | D

adjacentCoord:: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)


-- The maze

data Tile = Wall | Ground | Storage | Box | Player | Blank

maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank 
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
  
  
-- The state 

data State = St Coord Direction (List Coord)

initialBoxes :: List Coord
initialBoxes = Entry (C 2 2) (Entry (C 3 3) (Entry (C (-1) 0) Empty))

initialState :: State
initialState = St (C (-2) (-3)) U initialBoxes


-- Event handling


-- Drawing

wall, ground, storage, box :: Picture
wall    = colored grey (solidRectangle 1 1)
ground  = colored yellow (solidRectangle 1 1)
storage = colored red(solidCircle 0.4) & ground 
box     = colored brown (solidRectangle 1 1)
player  = solidPolygon [(0,0.4),(-0.4,-0.4),(0.4,-0.4)]

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Player = player
drawTile Blank = blank

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

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawPlayer:: Direction -> Picture
drawPlayer dir =  rotated (getAngle dir) (drawTile Player)

getAngle :: Direction -> Double
getAngle U = 0
getAngle R = 3*pi /2
getAngle D = pi
getAngle L = pi/2

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

--drawState :: State -> Picture
--drawState (St c dir) = atCoord c (drawPlayer dir) & pictureOfMaze

-- The complete activity

-- The general activity type

data Activity world = 
       Activity world 
       (Event -> world -> world) 
       (world -> Picture)

runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw) = activityOf state0 handle draw

-- Resetable activities

resetable :: Activity s -> Activity s
resetable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen
startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen
    handle' (KeyPress key) StartScreen = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

-- The main function


{-
tryGoTo :: Coord -> Coord -> Coord
tryGoTo from to
  | isOkToGo (maze to) = to
  | otherwise = from

isOkToGo :: Tile -> Bool
isOkToGo Ground = True
isOkToGo Storage = True
isOkToGo _ = False

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (St c dir)
  | key == "Right" = St (tryGoTo c (adjacentCoord R c)) R  
  | key == "Up"    = St (tryGoTo c (adjacentCoord U c)) U
  | key == "Left"  = St (tryGoTo c (adjacentCoord L c)) L
  | key == "Down"  = St (tryGoTo c (adjacentCoord D c)) D
handleEvent _ c = c
-}

main :: IO ()
main = drawingOf (pictureOfBoxes initialBoxes)
--main = runActivity (resetable (withStartScreen exercise2))