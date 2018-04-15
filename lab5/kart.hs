--1)
doFun = do
    putStrLn "Podaj imie: "
    s <- getLine
    putStrLn $ "Witaj " ++ s

--przepisac z uzyciem >>, >>=	

fun = putStrLn "Podaj imię: " >> getLine >>= \n -> putStrLn $ "Witaj " ++ n

--2)
--foldr (+) 0 ((*)<$>ZipList[1,2,3]<*>((+1)ZipList[4,5,6]))
--do czego sie ewaluuje?

--3)

data Tree1 a = Node a (Tree1 a) (Tree1 a)
             | Leaf
             deriving Show

instance Functor Tree1 where
    fmap f (Leaf) = Leaf
    fmap f (Node a lT rT) = Node (f a) (fmap f lT) (fmap f rT)

instance Foldable Tree1 where
    foldMap f Leaf           = mempty
    foldMap f (Node a lT rT) = f a `mappend` foldMap f lT `mappend` foldMap f rT


-- napisac Functor i Foldable (kolejnosc pre-order)

fun1 = getLine >>= \l1 -> return (l1 ++ l1) >> putStrLn l1

doFun1 = do
    l1 <- getLine
    return (l1 ++ l1)
    putStrLn l1

fun2 = getLine >>= \l1 -> getLine >>=  \l2 ->  print [l1,l2]

doFun2 = do
    l1 <- getLine
    l2 <- getLine
    print [l1, l2]