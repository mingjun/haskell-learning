-- run it as runhaskell <me>.hs < input.txt


-- this will end with an exception
readThemAll = do 
  text <- getLine
  putStrLn $ "read-> "++text
  n <- readThemAll
  return $ n+1

main0 = readThemAll

-- an alternative is using function interact
{-
    :info interact 
    interact :: (String -> String) -> IO () -- Defined in `System.IO'
-}
  
echoAll = interact (\s -> "read " ++ (show $ length s) ++ "chars -> " ++s)   

main  = echoAll