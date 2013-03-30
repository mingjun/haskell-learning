
{-
instance Ord Family where
	XuMingjun > XuMingjun = False
	XuMingjun > _ = True
	_ > XuMingjun = False
	...
-}

data Family = XuMingjun | DingManying | Penny
			deriving (Eq, Show, Read)

qsort :: Ord a =>  [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y| y<-xs, y<=x] ++ [x] ++ qsort [y| y<-xs, y>x]