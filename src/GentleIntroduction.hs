module GentleIntroduction where

getAndPutChar :: IO ()
getAndPutChar = 
    do  c <- getChar
        putChar c

getAndPutDialog :: IO ()
getAndPutDialog =
    do putStrLn "Enter a character"
       c <- getChar
       putStrLn ""
       putStrLn $ "You entered '" ++ [c] ++ "'"
    

ready :: IO Bool
ready = 
    do c <- getChar
       return $ c == 'y'

readyDialog :: IO ()
readyDialog = 
    do putStr "Are you ready? "
       r <- ready
       putStrLn ""
       if r 
          then putStrLn "Great to hear that!"
          else putStrLn "Sorry to hear that"