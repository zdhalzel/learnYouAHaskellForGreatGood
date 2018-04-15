module Types.Lockers where
  
import qualified Data.Map as Map

data Either' a b = Left' a | Right' b
  deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free 
  deriving (Show, Eq)

type Code = String
type Reason = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either Reason Code
lockerLookup lockerNum map = 
  case Map.lookup lockerNum map of
     Nothing -> Left $ "Locker number " ++ show lockerNum ++ " doesn't exist!"
     Just (state, code) -> if state /= Taken then Right code
     else Left $ "Locker " ++ show lockerNum ++ " is already taken!"

lockers :: LockerMap     
lockers = Map.fromList
  [(100,(Taken,"ZD39I"))
  ,(101,(Free,"JAH3I"))
  ,(103,(Free,"IQSA9"))
  ,(105,(Free,"QOTSA"))
  ,(109,(Taken,"893JJ"))
  ,(110,(Taken,"99292"))]
  
data Listt a = Emptty | Cons a (Listt a)
  deriving (Show, Read, Eq, Ord)
  
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)