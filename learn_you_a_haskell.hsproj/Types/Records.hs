module Types.Records where

data Person = Person 
  { firstName :: String
  , lastName :: String
  , age :: Int
  , height :: Float
  , phoneNumber :: String
  , flavor :: String
  } deriving (Show)
  
data Person' = Person'
  { firstName' :: String
  , lastName' :: String
  , age' :: Int
  } deriving (Eq, Show, Read)

data Car = Car 
  { company :: String
  , model :: String
  , year :: Int
  } deriving (Show)
  
tellCar :: Car -> String
tellCar (Car { company = c, model = m, year = y})
  = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
  
--data Maybe' a = Nothing' | Just' a

data Vector a = Vec a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vec i j k) (Vec l m n) = Vec (i+l) (j+m) (k+n)

vmult :: (Num t) => Vector t -> t -> Vector t
vmult (Vec i j k) m = Vec (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
scalarMult (Vec i j k) (Vec l m n) = i*l + j*m + k*n

data Day = Sunday
         | Monday 
         | Tuesday 
         | Wednesday 
         | Thursday 
         | Friday 
         | Saturday 
         deriving (Eq, Ord, Show, Read, Bounded, Enum)