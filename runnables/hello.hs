-- ghc hello.hs -o hello
size :: [Char] -> Integer
size [] = 0
size (x:xs) = 1 + size xs

main = do 
       putStrLn "Hello world!"
       message <- getLine
       let ss = show (size message)
       putStrLn $  "message[" ++ ss ++ "] = " ++ message
