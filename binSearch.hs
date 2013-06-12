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
