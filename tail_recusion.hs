
summ       [] = 0
summ (h:tail) = h + summ(tail)
--
ssumm [] = 0
ssumm (h:tail) = summ(tail)+h

-- 
f       [] n = n
f (h:tail) n = f tail (n+h)

summm list = f list 0

--
ff n [] = n
ff n (h:tail) = ff (n+h) tail

ssummm list = ff 0 list


-- what is the best?

