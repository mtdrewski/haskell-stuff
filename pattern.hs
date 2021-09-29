lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"   

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"  
charName c = "ok"
  
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  

tell :: (Show a) => [a] -> String 
tell [] = "The list is empty"
tell (x:[])= "The list has one element: " ++ show x
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++" and "++show y


length' :: Num p => [a] -> p
length' [] = 0
length' (_:xs) = 1+ length' xs


sum' :: Num p => [p] -> p
sum' []=0
sum' (x:xs) = x+ sum' xs

capital :: [Char] -> [Char]
capital "" = "Empty string!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> [Char]
bmiTell bmi
    | bmi <= 18.5 = "Niedowaga"
    | bmi <= 25.0 = "Waga prawidlowa"
    | bmi <= 30.0 = "Nadwaga"
    | otherwise = "Echh"

bmiCalc :: Fractional a => a -> a -> a
bmiCalc weight height = weight/height^2

max' :: Ord p => p -> p -> p
max' a b
    | a>b = a
    | otherwise = b
--max' a b | a > b = a | otherwise = b  -- working as welll

bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0)  


initials :: [Char] -> [Char] -> [Char]
initials firstName lastName = [f]++"."++[l]++"."
    where (f:_) = firstName
          (l:_) = lastName

calcBmis :: Fractional a => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w,h)<- xs]
    where bmi weight height = weight/height^2

cylinder :: Floating a => a -> a -> a
cylinder h r = 
    let sideArea = 2*pi *r*h
        topArea = pi * r^2
    in sideArea + topArea*2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2] 


y :: Integer
y= y+1