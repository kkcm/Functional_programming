-- Napisz co to jest funktor

-- Opisz sposób obsługi I/O w haskelu

-- Napisać definicję struktury Person gdzie firstName i lastName to String, a age to Integer

data Person a = MkPerson {firstName :: String, lastName :: String, age :: Integer}

--Zaimplementuj instancję Eq dla:

data MyType = C1 (Bool, Int) | C2 Int | C3 Double
instance Eq MyType where
    (==) (C1 (b1, i1)) (C1 (b2, i2)) = b1==b1 && i1==i2
    (==) (C2 i1) (C2 i2) = i1==i2
    (==) (C3 d1) (C3 d2) = d1==d2  

-- Mając dany typ:

data BinTree a = NodeBT (BinTree a) (BinTree a)
              | Leaf a
              deriving (Show)

-- Napisz przykładowe implementacje dla obydwu konstruktorów

t1 = Leaf 6
t2 = NodeBT (NodeBT (Leaf 3) (Leaf 7)) (Leaf 12)

-- Zaimplementuj funkcję mapującą BinTree

treeMapBT :: (a -> b) -> BinTree a -> BinTree b
treeMapBT f (Leaf a)   = Leaf (f a)
treeMapBT f (NodeBT l r) = NodeBT (treeMapBT f l) (treeMapBT f r)

depthOfBT :: BinTree a -> Int
depthOfBT (Leaf a) = 1
depthOfBT (NodeBT l r) = 1 + max (depthOfBT l) (depthOfBT r)

--Znaleźć 4 błędy w:

--newtype Foo a = MkFoo { value :: a, name :: String }
--instance Show (Foo a) where
  --show Foo{ v = value, n = name } = "Name: " ++ n ++ " with " ++ show v

data Foo a = MkFoo {value :: a, name :: String} 
instance Show (Foo a) where
    show MkFoo{ value = v, name=n } = "Name: " ++ n ++ " with " ++ show v


data Tree1 a = Node [Tree1 a] [Tree1 a] | Leaf1 a

--maxValue :: Ord a -> Tree1 a -> a
--maxValue (Leaf1 a) = a
--maxValue (Node l r) = max  (maxValue Tree1 l, maxValue Tree1 r)




