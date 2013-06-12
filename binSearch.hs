-- chop here is binary search
chop v [] = -1

chop v [w] = case v==w of
             True   ->  0
             False  -> -1

chop v list | v == w = m
            | v < w = chop v headHalf
            | otherwise = (\x -> if x>=0 then m+1+x else x ) $ chop v tailHalf
            where
                  m = (length list - 1) `div` 2 ;
                  (headHalf, (w:tailHalf)) = splitAt m list
