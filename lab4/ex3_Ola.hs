-- sumBinTree $ NodeBT 1 EmptyBT EmptyBT
-- sumBinTree (NodeBT 1 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 3 EmptyBT EmptyBT))
-- sumBinTree $ NodeBT 1 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 3 EmptyBT EmptyBT)

data BinIntTree = EmptyIntBT | IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

-----------------------------------------------------

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

-----------------------------------------------------

sumBinLeaves :: BinTree a -> a
sumBinLeaves EmptyBT = 0
--sumBinLeaves (NodeBT n EmptyBT EmptyBT) = n
sumBinLeaves (NodeBT n lt rt) =
  if (NodeBT n EmptyBT EmptyBT) then n + sumBinLeaves lt + sumBinLeaves rt
    else NodeBT n (sumBinLeaves lt) (sumBinLeaves rt)

-----------------------------------------------------

depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT a lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

-----------------------------------------------------

flattenBT :: BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
flattenBT EmptyBT = []
flattenBT (NodeBT n lt rt) = flattenBT lt ++ [n] ++ flattenBT rt -- inorder
-- preorder: [n] + flattenBT lt + flattenBT rt
-- postorder: flattenBT lt + flattenBT rt + [n]

-----------------------------------------------------

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

-----------------------------------------------------

insert :: (Ord a) => a -> BinTree a -> BinTree a -- insert element into BinTree
insert x EmptyBT = NodeBT x EmptyBT EmptyBT
insert x (NodeBT a lt rt)
  | x == a  = NodeBT x lt rt
  | x < a   = NodeBT a (insert x lt) rt
  | x > a   = NodeBT a lt (insert x rt)

-----------------------------------------------------

--list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)
