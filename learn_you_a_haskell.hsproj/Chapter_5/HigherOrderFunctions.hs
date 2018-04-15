module Chapter_5.HigherOrderFunctions where
  
max' :: (Ord a) => a -> a -> a
max' a b
  | a >= b = a
  | otherwise = b
  
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' x = compare 100 x

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

divideTenBy :: (Floating a) => a -> a
divideTenBy = (10/)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = flip elem ['A'..'Z']

subtract5from :: (Num i) => i -> i
subtract5from = subtract 5

subtractFrom5 :: (Num i) => i -> i
subtractFrom5 = flip subtract 5

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f $ f x

zipWith' :: (x -> y -> z) -> [x] -> [y] -> [z]
zipWith' f [] any = []
zipWith' f any [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--flip' :: (a -> b -> c) -> (b -> a -> c)
--flip' f = g
--  where g x y = f y x

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) 
  | p x       = x : filter' p xs
  | otherwise = filter' p xs 

notNull x = not (null x)

isLower :: Char -> Bool
isLower = flip elem ['a'..'z']

isUpper :: Char -> Bool
isUpper x = elem x ['A'..'Z']

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort $ filter (<= x) xs
      largerSorted  = quicksort $ filter (> x) xs
  in smallerSorted ++ [x] ++ largerSorted
  
largestDivisible :: (Integral a) => a
largestDivisible = head $ filter p [100000,99999..]
  where p x = mod x 3829 == 0
  
largestDiv' :: Integer -> Integer
largestDiv' x = head (filter ld [99999,99998..])
  where ld element = mod element x == 0
  
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | even n = n:collatz (div n 2)
  | odd  n = n:collatz (n*3 + 1)
  
numLongChains :: Int
numLongChains = length $ filter isLong $ map collatz [1..100]
  where isLong xs = length xs > 15
  
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) 
  (map collatz [1..100]))
 
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

