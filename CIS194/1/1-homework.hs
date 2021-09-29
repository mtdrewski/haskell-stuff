{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

lightCircle c x y = colored c (translated x y (solidCircle 1))
topCircle c = lightCircle c 0 2.5
midCircle c = lightCircle c 0 0 
botCircle c = lightCircle c 0 (-2.5)
frame = rectangle 2.5 7.5

trafficLightState top mid bot = topCircle top & midCircle mid & botCircle bot & frame 

trafficLight :: String -> Picture
trafficLight "Proceed" = trafficLightState black black green
trafficLight "NowHalt" = trafficLightState black yellow black
trafficLight "Halt" = trafficLightState red black black
trafficLight "NowProceed" = trafficLightState red yellow black

trafficController :: Double -> Picture
trafficController t
  | timeUnit <4 = trafficLight "Proceed"
  | timeUnit == 4 =trafficLight "NowHalt"
  | timeUnit <9 = trafficLight "Halt"
  | otherwise = trafficLight "NowProceed"
  where timeUnit = round (t/2) `mod` 10 
exercise1 = animationOf trafficController 


tree :: Integer -> Double -> Picture
tree n x
    | n == 0 = colored yellow (solidCircle x)
    | otherwise = polyline [(0,0),(0,1)] & translated 0 1 (
    rotated (pi/10) (tree (n-1) x) & rotated (- pi/10) (tree (n-1) x))
    
treeBlossom :: Double -> Picture
treeBlossom t
  | t <=10 = tree 8 (t/50)
  | otherwise = tree 8 0.2
exercise2 = animationOf treeBlossom




tile c = colored c(solidRectangle 1 1)
wallTile = tile black 
groundTile = tile yellow 
boxTile = tile brown 
storageTile = (colored red(solidCircle 0.4)) & groundTile 

drawTile :: Integer -> Picture
drawTile tileID
  | tileID == 1 = wallTile
  | tileID == 2 = groundTile
  | tileID == 3 = storageTile
  | tileID == 4 = boxTile
  | otherwise = blank

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

pictureOfMaze :: Integer -> Integer -> Picture
pictureOfMaze x y
  | x == (-10) && y == (-10) = blank
  | x == (-10) = (pictureOfMaze 10 (y-1)) & (translated x' y' (drawTile (maze x y))) 
  | otherwise = (pictureOfMaze (x-1) y) & (translated x' y' (drawTile (maze x y)))
  where x' = fromIntegral x
        y' = fromIntegral y

main :: IO ()
main= drawingOf(pictureOfMaze 10 10)

