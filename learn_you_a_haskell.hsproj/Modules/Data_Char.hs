module Modules.Data_Char where
  
import Data.Char
import Data.Function (on)
import Data.List (groupBy)


encode :: Int -> String -> String
encode shift msg = 
  let ords = map ord msg
      shifted = map (+ shift) ords
  in map chr shifted
  
encode' :: Int -> String -> String
encode' shift = map $ chr . (+ shift) . ord

decode :: Int -> String -> String
decode shift = encode' (negate shift)