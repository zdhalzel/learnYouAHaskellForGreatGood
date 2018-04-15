module HS3 where
factoriaL :: Integer -> Integer
factoriaL n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

green :: Double -> Double
green x = x + 2

-- Ch.3 Syntax

-- Syntax in Functions
lucky :: Int -> String

lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"


sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!" 
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


addVectors :: (RealFloat d) => (d, d) -> (d, d) -> (d, d)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
-- this way works, but is not as clear:
-- addVectors a b = (fst a + fst b, snd a + snd b)


-- functions for triples
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z


-- more pattern matching
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x


tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y 
tell (x:y:_) = "This list is long, the first two elements are " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty String, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- doesn't cover all possible list sizes
badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z


-- As-pattern
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- Guards, Guards!
bmiTell :: Double -> Double -> String
bmiTell weight height 
  | bmi <= skinny = "You're underweight, eat more!"
  | bmi <= normal = "Looking good!"
  | bmi <= overweight = "You're overweight. Let's workout together!"
  | otherwise   = "You're obese. Go see a doctor."
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        overweight = 30.0

max' :: (Ord z) => z -> z -> z
max' p q
  | p <= q    = q
  | otherwise = p

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
  | a == b  = EQ
  | a <= b = LT
  | otherwise = GT
  

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
  where niceGreeting = "Hello"
greet "Fernando" = niceGreeting ++ " Fernando!"
  where niceGreeting = "Hello"
greet name = badGreeting ++ " " ++ name ++ "."
  where badGreeting = "Oh! Pfft. It's you."
        

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname
       

-- Functions in where Blocks
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2
  
-- let it Be
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + topArea * 2


calcBmis' :: (RealFloat d) => [(d, d)] -> [d]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w/h^2, bmi > 25.0]
-- (w,h) <- xs 
-- this part of the list comprehension is called the generator
-- the generator is defined prior to the let binding


-- case Expressions
head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty Lists"
                       (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton."
                                               xs -> "a longer list."
                                               
describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
  where what []  = "empty."
        what [x] = "a singleton list."
        what xs  = "a longer list."
