module Modules.Part2 where
  
import Data.List
import Data.Function (on)
  
sameSign :: (Num a, Ord a) => a -> a -> Bool
sameSign = on_ (==) (> 0)
--sameSign x y = (x > 0) == (y > 0)

on_ :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on_ f g = \ x y -> f (g x) (g y)