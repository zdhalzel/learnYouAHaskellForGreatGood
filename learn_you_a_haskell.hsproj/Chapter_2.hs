module Chapter_2 where
  
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x 
  = if x > 100 then x else doubleMe x
                                 
doubleSmallNumber' x
  = (if x > 100 then x else doubleMe x) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

lostNumbers = [4,8,15,16,23,42]

b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, elem c ['A'..'Z']]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]