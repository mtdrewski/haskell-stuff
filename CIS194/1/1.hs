{-# LANGUAGE OverloadedStrings #-}
import CodeWorld


botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))
topCircle c = colored c (translated 0   1.5  (solidCircle 1))
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True  = botCircle green & topCircle black & frame
trafficLight False = botCircle black & topCircle red   & frame


--ourPicture :: Picture
--ourPicture = trafficLight False

{-trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod` 2 == 0 = trafficLight True
  | otherwise                = trafficLight False
main = animationOf trafficController
-}

lights :: Integer -> Picture
lights 0 = blank
lights n = trafficLight True & translated 3 0 (lights (n-1))

spread :: Picture -> Double -> Integer -> Picture
spread _ _ 0 = blank
spread pic dx n = pic & translated dx 0 (spread pic dx (n-1))

ourPicture = spread (trafficLight True) 6 4

tree :: Integer -> Picture
tree 0 = blank
tree n = polyline [(0,0),(0,1)] & translated 0 1 (
    rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))


main :: IO ()
main =  drawingOf (tree 9)

