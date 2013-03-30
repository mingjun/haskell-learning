-- hello world

increase :: Integer -> Integer
increase a = a+1

twice f x = f (f x)

(+++) :: Integer -> Integer -> Integer
(+++) x  y = x + y
-- here we have ( 2 +++ 3 == 5 )



-- insert x into xs at the position of n
insert :: a -> [a] -> Int -> [a]
insert x xs n = (fst pair) ++ [x] ++ (snd pair)
				where pair = splitAt n xs

-- insert x into xs at any position (encapsulate all instances in a List)
insertAnywhere :: a -> [a] -> [[a]]
insertAnywhere x xs = map (insert x xs) [0..n]
				where n = length xs

-- permutation
-- permutation "abc" ~= ["abc","acb","bac","bca","cab","cba"]
permutation :: [a] -> [[a]]
permutation [] = [[]]
permutation (h:xs) = concat ( map (insertAnywhere h) (permutation xs) )

--
ss a b c = a+b+c
--
xmj = \x -> case (2*x) of
                0 -> 1
                _ -> 2

dmy x | 2*x == 0 = 1
      | otherwise = 3
