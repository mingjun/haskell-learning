-- Postion & Move
type Position = (Int, Int)
data Move = Left' | Right' | Up | Down

move :: Move -> Position -> Position
move Left'  (x,y) = (x-1,y)
move Right' (x,y) = (x+1,y)
move Up     (x,y) = (x,y+1)
move Down   (x,y) = (x,y-1)

-- Shape
data Shape = Circle Float | Rect Float Float
           deriving (Eq, Ord, Show, Read) -- classes
{-so that it adding a group of functions == > < show read etc. -}

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x*y

{- java code

interface Shape{
    float area();
}
class Circle{
    float r;
    float area(){return Math.PI*r*r;}
}
class Rect{
    float x, y;
    Rect(float x, float y){this.x=x; this.y=y;}
    float area(){return x*y;}
    static Rect square(float n){return new Rect(n,n);}
}

-}

{- predef
data Maybe a = Nothing | Just a
-}
safediv :: Integer -> Integer -> Maybe Integer
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

-- Nature Number
data Nat = Zero | Succ Nat
nat2int :: Nat -> Integer
nat2int Zero = 0
nat2int (Succ n) = nat2int n + 1

int2nat :: Integer -> Nat
int2nat 0 = Zero
int2nat (n+1) = Succ (int2nat n)



-- quick sort
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = filter (<x) (sort xs) ++ [x] ++ filter (>=x) (sort xs)

{-
class Show a => MyClass a
    where say :: a -> String
          say x = (show x) ++ "said I"

data MyChar = A | C | B | D
              deriving (Eq, Ord, Show, Read, MyClass)
-}

-- define a new class
class Animal a
    where hello :: a -> String

-- define two new type
data Human = XuMingjun | DingManying
               deriving (Eq, Show, Read)
data Pet = Penny | WangCai
               deriving (Eq, Show, Read)

-- implement declared functions
instance Animal Human where
    hello x = "\"Hello!\", said " ++ (show x)

instance Animal Pet where
    hello x = "\"Won Won!\", said " ++ (show x)
