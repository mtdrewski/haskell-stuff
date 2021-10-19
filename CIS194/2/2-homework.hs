{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
 
wall, ground, storage, box :: Picture
wall    = colored grey (solidRectangle 1 1)
ground  = colored yellow (solidRectangle 1 1)
storage = colored red(solidCircle 0.4) & ground 
box     = colored brown (solidRectangle 1 1)
player  = solidPolygon [(0,0.4),(-0.4,-0.4),(0.4,-0.4)]

data Tile = Wall | Ground | Storage | Box | Player | Blank
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

data Coord = C Integer Integer
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = atCoord(C r c) (drawTile (maze (C r c)))

maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank 
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

data Direction = R | U | L | D

adjacentCoord:: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)


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

data State = St Coord Direction

startState :: State
startState = St (C (-2) (-3)) U

drawPlayer:: Direction -> Picture
drawPlayer dir =  rotated (getAngle dir) (drawTile Player)

getAngle :: Direction -> Double
getAngle U = 0
getAngle R = 3*pi /2
getAngle D = pi
getAngle L = pi/2

drawState :: State -> Picture
drawState (St c dir) = atCoord c (drawPlayer dir)  & pictureOfMaze

resetableActivityOf ::
    world ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
resetableActivityOf state handle draw  = activityOf state handle1 draw
  where handle1 (KeyPress key) _ | key == "Esc" = state
        handle1 key st = handle key st
        

main :: IO ()
main = resetableActivityOf startState handleEvent drawState
