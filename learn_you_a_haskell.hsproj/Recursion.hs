module Recursion where
  
fib :: Int -> Int
fib n
  | n <= 0 = 1
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fib (n-1) + fib (n-2)


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
--maximum' (x:xs)
--  | x > maxTail = x
--  | otherwise = maxTail
--  where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x:replicate' (n-1) x
  -- use guards when testing for a boolean condition
 

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' any [] = []
zip' [] any = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = elem' a xs
  
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      largerSorted  = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ largerSorted

--quicksort :: (Ord a) => [a] -> [a]
--quicksort [] = []
--quicksort (x:xs) = 
--  let smallerOrEqual = [a | a <- xs, a <= x]
--      larger         = [a | a <- xs, a > x]
--  in quicksort smallerOrEqual ++ [x] ++ quicksort   larger

