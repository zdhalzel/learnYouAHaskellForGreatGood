module Modules.Data_Map where
  
import Data.Char
import qualified Data.Map as Map

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs) = if key == k
                         then Just v
                         else findKey key xs
                         
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\ (k,v) acc -> if key == k then Just v else acc) Nothing

-- M.fromList :: (Ord k) => [(k,v)] -> M.Map k v

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\ (k,v) acc -> Map.insert k v acc) Map.empty

phoneBookToMap :: (Ord k) => [(k,a)] -> Map.Map k [a]
--phoneBookToMap = Map.fromList . map (\ (k,v) -> (k,[v]))
phoneBookToMap = Map.fromListWith (++) . map (\ (k,v) -> (k,[v]))

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = elem (name,pnumber) pbook

type AssocList k v = [(k,v)]

type IntMap v = Map.Map Int v

type IntMap' = Map.Map Int

-- data Either a b = Left a | Right b 
--  deriving (Eq, Ord, Read, Show)