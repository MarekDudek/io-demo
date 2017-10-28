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

myGetLine :: IO String
myGetLine = 
    do c <- getChar
       if c == '\n'
        then return ""
        else 
            do l <- myGetLine
               return (c:l)

helloDialog :: IO ()
helloDialog = 
    do putStr "Enter your name: "
       name <- getLine
       putStrLn $ "Hello " ++ name ++ "!"

todoList :: [IO ()]
todoList = [
    putChar 'a',
    do putChar 'b'
       putChar 'c',
    do c <- getChar
       putChar c
  ]

doTodos = sequence_ todoList
