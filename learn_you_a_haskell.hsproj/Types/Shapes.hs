module Types.Shapes
  ( Point(..)
  , Shape(..)
  , surface
  , nudge
  , baseCircle
  , baseRect
  ) where
  
data Shape
  = Circle Point Float 
  | Rect Point Point
  deriving (Show)
  
data Point = Pt Float Float deriving (Show)
  
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rect (Pt x1 y1) (Pt x2 y2)) = w * h
  where w = distance x1 x2
        h = distance y1 y2
        distance a b = (abs $ a - b)
        
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Pt x y) r) a b 
  = Circle (Pt (x+a)(y+b)) r
nudge (Rect (Pt x1 y1) (Pt x2 y2)) a b 
  = Rect (Pt (x1+a)(y1+b)) (Pt (x2+a)(y2+b))
      
baseCircle :: Float -> Shape
baseCircle r = Circle (Pt 0 0) r

baseRect :: Float -> Float -> Shape
baseRect w h = Rect (Pt 0 0) (Pt w h)

