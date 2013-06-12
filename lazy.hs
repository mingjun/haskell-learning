qsort [] = []
qsort (x:xs) = (qsort aa) ++ [x] ++ (qsort bb)
  where 
    aa = filter (< x) xs
    bb = filter (>= x) xs  

header _ [] = []
header 0 _ = []
header x (h:xs) = [h] ++ (header (x-1) xs)

-- how about header 5 (qsort[1..1000000])  
