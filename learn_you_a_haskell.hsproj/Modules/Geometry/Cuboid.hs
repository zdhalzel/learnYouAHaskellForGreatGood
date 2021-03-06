module Modules.Geometry.Cuboid 
  ( volume
  , area
  ) where
    
volume :: Float -> Float -> Float -> Float
volume a b c = rectArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = (rectArea a b + rectArea a c + rectArea c b) * 2

rectArea :: Float -> Float -> Float
rectArea a b = a * b

