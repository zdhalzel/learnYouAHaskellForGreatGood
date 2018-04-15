module Chapter_5.Folds where

-- if the list is empty, the result is z; 
-- else we recurse immediately, making the new 
-- initial value the result of combining the old 
-- initial value with the first element.
foldl_ f acc []     = acc                  
foldl_ f acc (x:xs) = foldl_ f (f acc x) xs
--                     foldl_ + (+ 0 1) [2]
--                     foldl_ + (+ (+ 0 1) 2) [] 
--                     foldl_ + 3 [] == 3
 
-- if the list is empty, the result is init;
-- else apply f to the first element 
-- and the result of folding the rest
foldr_ f init []     = init
foldr_ f init (x:xs) = f x (foldr_ f init xs) 
--                     1 + (foldr_ (+) 0 [2])
--                     1 + 2 + (foldr_ (+) 0 [])
--                     1 + 2 + 0 == 3
 
-- sum experiments
suml :: (Num a) => [a] -> a
suml = foldl (+) 0

sumr :: (Num a) => [a] ->a
sumr = foldr (+) 0

suml' :: (Num a) => [a] -> [a]
suml' = scanl (+) 0

sumr' :: (Num a) => [a] -> [a]
sumr' = scanr (+) 0

------

suml1 :: (Num a) => [a] -> a
suml1 = foldl1 (+)
--sum'' = foldl (+) 0

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\ x acc -> f x : acc) []

map''' :: (a -> b) -> [a] -> [b]
map''' f = foldl (\ acc x -> acc ++ [f x]) []


eleml :: (Eq a) => a -> [a] -> [Bool]
eleml item = scanl (\ acc x -> if x == item then True else acc) False
-- foldl_ f acc []     = acc                  
-- foldl_ f acc (x:xs) = foldl_ f (f acc x) xs



elemr :: (Eq a) => a -> [a] -> [Bool]
elemr item = scanr (\ candidate accumulator -> if candidate == item then True else accumulator) False

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max
-- foldr1 (\ x acc -> if x > acc then x else acc

reversel :: [a] -> [a]
reversel = foldl (\ xs x -> x:xs) []

reverser :: [a] -> [a]
reverser = foldr (\ x xs -> xs ++ [x]) []

reverser' :: [a] -> [[a]]
reverser' = scanr (\ x xs -> [x] ++ xs) []

reversel' :: [a] -> [[a]]
reversel' = scanl (\ xs x -> x:xs) []

reversef :: [a] -> [a]
reversef = foldl (flip (:)) []
-- whoa

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\ x xs -> if p x then x:xs else xs) []

headr :: [a] -> a
headr = foldr1 (\ x acc -> x)

last1 :: [a] -> a
last1 = foldl1 (\ acc x -> x)

diff_right = foldr (\ x acc -> x - acc) 0
diff_right' = foldr (\ x acc -> acc - x) 0
diff_left = foldl (\ acc x -> acc - x) 0
diff_left' = foldl (\ acc x -> x - acc) 0

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

andS :: [Bool] -> [Bool]
andS xs = scanr (&&) True xs

sqrtSums z = length (takeWhile (< z) (scanl1 (+) (map sqrt [1..]))) + 1

($$) :: (a -> b) -> a -> b
f $$ x = f x

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \ x -> f $ g x

fn x = ceiling (negate (tan (cos (max 50 x))))

fn' = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]

