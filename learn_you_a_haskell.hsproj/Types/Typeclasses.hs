module Types.Typeclasses where
  
import Types.Trees
  
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x == y = not (x /= y)
--   x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green
--  deriving (Show)

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False
  
instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"
  
-- instance (Eq m) => Eq (Maybe m) where
--   Just x == Just y = x == y
--   Nothing == Nothing = True
--   _ == _ = False

class YesNo a where
  yesno :: a -> Bool
  
instance YesNo Int where
  yesno 0 = False
  yesno _ = True
  
instance YesNo [a] where
  yesno [] = False
  yesno _ = True
  
instance YesNo Bool where
  yesno = id
  
instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False
  
instance YesNo (Tree a) where
  yesno Emptree = False
  yesno _ = True
  
instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True
  
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = 
  if yesno yesnoVal then yesResult else noResult
  


