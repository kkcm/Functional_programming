data CartInt2DVec = MkCartInt2DVec Int Int

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

type X = Int
type Y = Int

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- xCoord'' :: Cart2DVec''  a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' { y = yVal, x = _}) = yVal

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

data ThreeColors =  Blue |
                    White |
                    Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

data Cart3DVec a = MkCart3DVec a a a

xCoord3D :: Cart3DVec a -> a
xCoord3D (MkCart3DVec x _ _ ) = x

yCoord3D :: Cart3DVec a -> a
yCoord3D (MkCart3DVec _ y _ ) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (MkCart3DVec _ _ z ) = z

data Cart3DVec' a = MkCart3DVec' {x3D::a, y3D::a, z3D::a}

data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r) = 3.14*r^2
area (Rectangle x y) = x*y

data TreeR a = EmptyT |
               Node a (TreeR a) (TreeR a)
               deriving Show

rootValue :: TreeR a -> a
rootValue EmptyT      = error "rootValue': the empty tree has no root!"
rootValue (Node a l r)  = a

data TrafficLights =  RedT |
                      Yellow |
                      Green

data Action = Ride |
              Prepare |
              Stop
              deriving Show

actionFor :: TrafficLights -> Action
actionFor RedT = Stop 
actionFor Yellow = Prepare 
actionFor  Green = Ride
