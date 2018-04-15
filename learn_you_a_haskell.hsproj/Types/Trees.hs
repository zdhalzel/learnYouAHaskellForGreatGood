module Types.Trees where
  
data Tree a = Emptree | Node a (Tree a) (Tree a) 
  deriving (Show, Read, Eq)
  
singleton :: a -> Tree a
singleton x = Node x Emptree Emptree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Emptree = singleton x
treeInsert x (Node val left right)
  | x == val = Node x left right
  | x < val  = Node val (treeInsert x left) right
  | x > val  = Node val left (treeInsert x right)
  
 
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x Emptree = False
treeElem x (Node val left right)
  | x == val = True
  | x < val = treeElem x left
  | x > val = treeElem x right