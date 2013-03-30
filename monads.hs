-- parse functions
f :: String -> Integer
f x = read x

g :: Integer -> String
g y = show y


type Parser a = String -> [(a,String)]

return' :: a -> Parser a
return' v = \inp -> [(v,inp)]

failure' :: Parser a
failure' = \inp -> []


item :: Parser Char
item = \inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x,xs)]

parser :: Parser a -> String -> [(a,String)]
parser p inp = p inp

(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>>= f = \inp -> case parser p inp of
                   [] -> []
                   [(v,out)] -> parser (f v) out


-- IO &  Monadic

-- do statement looks like imperative language
put3Chars :: [Char] -> IO ()
put3Chars (x:(y:(z:xs))) =
    do putChar x
       putChar y
       putChar z

get3Chars :: IO String
get3Chars = do x <- getChar
               y <- getChar
               z <- getChar
               return [x,y,z]

getChars :: Int -> IO String
getChars  0    = do return []
getChars (n+1) = do x <- getChar
                    xs <- getChars n
                    return (x:xs)
-- >> functions
put2Chars :: [Char] -> IO ()
put2Chars (x:(y:xs)) =
    putChar x >> putChar y

-- >>= functons
get2Chars :: IO String
get2Chars = getChar >>= f
        where f x = getChar >>= g
                    where g y = return [x,y]
