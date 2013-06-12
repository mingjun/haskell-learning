-- chop here is binary search
chop v [] = -1

chop v [w] = case v==w of
             True   ->  0
             False  -> -1

chop v [x,y] | v == x = 0
             | v == y = 1
             | otherwise = -1

chop v list | v == w = m
            | v < w = chop v headHalf
            | otherwise = m + 1 + (chop v tailHalf)
            where m = (length list - 1) `div` 2
                  (headHalf, (w:tailHalf)) = splitAt m list


