{-# LANGUAGE ScopedTypeVariables #-}

module GentleIntroduction where

import Control.Exception
import System.IO
import System.IO.Error

-- Basic I/O Operations

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

-- Programming With Actions

doTodos = sequence_ todoList

-- Exception Handling

getInt :: IO Int
getInt = 
    do line <- getLine
       return $ read line

getIntDialog :: IO Int
getIntDialog =
    do putStr "Enter number: "
       getInt `catch` handler
       where handler :: IOError -> IO Int
             handler e = return 0

getIntDialog2 :: IO Int
getIntDialog2 =
    do putStr "Enter number: "
       getInt `catch` (\(e :: SomeException) -> return 0)

getIntDialog3 :: IO Int
getIntDialog3 =
    do putStr "Enter number: "
       getInt `catch` (\e -> 
        do putStrLn $ show (e :: IOError)
           return 0
        )

getIntDialog4 :: IO Int
getIntDialog4 =
    do putStr "Enter number: "
       getInt `catch` handler
       where handler e = if isEOFError e then return 0 else return 1

-- Files, Channels and Handles

doSthWithFile :: IO ()
doSthWithFile =
    do handle <- openFile "test/some-file.txt" ReadMode
       putStrLn $ show handle
       c <- hGetChar handle
       putStrLn $ show c
       contents <- hGetContents handle
       putStr contents
       hClose handle
       return ()

writeToFile :: FilePath -> IO ()
writeToFile p = 
    do h <- openFile "test/test.txt" WriteMode
       hPutChar h 'a'
       hPutStrLn h ""
       hPutStr h "this is a line of text"
       hPutChar h '\n'
       hClose h
       return ()

fromFileOrDefault :: FilePath -> IO ()
fromFileOrDefault path = (
    do h <- openFile path ReadMode
       c <- hGetContents h
       putStr c
    ) `catch` defaultValue
    where      
          defaultValue :: IOError -> IO ()
          defaultValue e = putStrLn $ displayException e