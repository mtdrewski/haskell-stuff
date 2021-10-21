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

allList :: List Bool -> Bool
allList Empty = True
allList (Entry b bs) = b && allList bs 

-- Coordinates

data Coord = C Integer Integer

data Direction = R | U | L | D

adjacentCoord:: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)

eqCoord::Coord ->Coord->Bool
eqCoord (C x1 y1) (C x2 y2) = (x1==x2) && (y1==y2) 

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo c1 c2 c | c1 `eqCoord` c = c2  
                   | otherwise      = c

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
  
noBoxMaze :: Coord -> Tile 
noBoxMaze c = case maze c of
  Box -> Ground
  t -> t
  
mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty c' = noBoxMaze c'
mazeWithBoxes (Entry c cs) c'
  | eqCoord c c' = Box
  | otherwise = mazeWithBoxes cs c'
  
-- The state 
data State = St Coord Direction (List Coord)

initialBoxes :: List Coord
initialBoxes = go (-10) (-10)
  where
    go 11 11 = Empty
    go x 11 = go (x+1) (-10)
    go x y = case (maze (C x y)) of 
      Box -> (Entry (C x y) (go x (y+1)))
      _ -> go x (y+1)

initialState :: State
initialState = St (C (-2) (-3)) U initialBoxes

isWon :: State -> Bool
isWon (St _ _ boxList) = allList boolList 
                         where boolList = mapList isOnStorage boxList
                              
isOnStorage :: Coord -> Bool
isOnStorage c = case noBoxMaze c of
                        Storage -> True
                        _ -> False

-- Event handling

isOkToGo :: Tile -> Bool
isOkToGo Ground = True
isOkToGo Storage = True
isOkToGo _ = False

tryGoTo :: State -> Direction -> State
tryGoTo (St from _ boxList) dir
    = case currentMaze to of
      Box -> case currentMaze beyond of
        Ground -> movedState
        Storage -> movedState
        _ -> didn'tMove
      Ground -> movedState
      Storage -> movedState
      _ -> didn'tMove
    where to = adjacentCoord dir from
          beyond = adjacentCoord dir to
          currentMaze = mazeWithBoxes boxList
          movedBoxList = mapList (moveFromTo to beyond) boxList
          movedState = St to dir movedBoxList
          didn'tMove = St from dir boxList
  
handleEvent :: Event -> State -> State
handleEvent _ st
  | isWon st == True = st
handleEvent (KeyPress key) s
  | key == "Right" = tryGoTo s R
  | key == "Up"    = tryGoTo s U
  | key == "Left"  = tryGoTo s L
  | key == "Down"  = tryGoTo s D
handleEvent _ st = st

-- Drawing

wall, ground, storage, box, player :: Picture
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

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = atCoord(C r c) (drawTile (noBoxMaze (C r c)))

drawPlayer:: Direction -> Picture
drawPlayer dir =  rotated (getAngle dir) (drawTile Player)

getAngle :: Direction -> Double
getAngle U = 0
getAngle R = 3*pi /2
getAngle D = pi
getAngle L = pi/2

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

endScreen :: State -> Picture
endScreen st = case isWon st of
                    True -> scaled 3 3 (lettering "You won!")
                    False -> blank

drawState :: State -> Picture
drawState (St c dir initialBoxes) = endScreen (St c dir initialBoxes) & atCoord c (drawPlayer dir) 
                                   & pictureOfBoxes initialBoxes & pictureOfMaze
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
main :: IO ()
main = runActivity (resetable (withStartScreen (Activity initialState handleEvent drawState)))