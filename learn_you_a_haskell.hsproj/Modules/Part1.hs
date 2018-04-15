module Modules.Part1 where
  
import Data.List

import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

numUniques' :: (Eq a) => [a] -> Int
numUniques' = (\ xs -> length (nub xs))

tuplify w = (head w, length w)

wordNums :: String -> [(String,Int)]
wordNums = map tuplify . group . sort . words

wordNums' :: String -> [(String,Int)]
wordNums' xs = map tuplify (group (sort (words xs)))

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = 
  any (isPrefixOf needle) $ tails haystack

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False $ tails haystack