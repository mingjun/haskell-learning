-- chop here is binary search
chop v [] = -1

chop v [w] = case v==w of
               True   ->  0
               False  -> -1

chop v list | v == w = m
            | v < w = chop v headHalf
            | otherwise = m+1 `positiveAdd` chop v tailHalf
            where
              m = (length list - 1) `div` 2 ;
              (headHalf, (w:tailHalf)) = splitAt m list
              positiveAdd x y = if x < 0 || y < 0 then -1 else x+y

-- bisect: divide the list in to half-half
bisect [] = ([], [])
bisect list = splitAt m list where
  m = length list  `div` 2

merge [] list = list
merge list [] = list
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

msort [] = []
msort [x] = [x]
msort list = msort lowHalf `merge` msort highHalf
             where
               (lowHalf, highHalf) = bisect list

rank i list
  | i <= 0          = error "rank value must be positive"
  | i > length list = error "rank out of list count"
  | otherwise       = safeRank i list

safeRank i [x] = x
safeRank i (x:xs)
  | i < rankX  = safeRank i part1
  | i > rankX  = safeRank (i-rankX) $ filter (>=x) xs
  | otherwise = x
  where part1 = filter (<x) xs
        len1 = length part1
        rankX = len1+1

median [] = error "no median in blank list"
median list = safeRank (length list `div` 2) list