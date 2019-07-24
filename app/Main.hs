{-# LANGUAGE OverloadedStrings #-}

module Main where

import BF
import System.Exit
import Control.Monad
import System.Console.ANSI
import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Char
import System.Environment

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    
    args <- getArgs
    if null args
    then repl
    else do
        content <- BS.readFile (args !! 0)
        hSetBuffering stdin NoBuffering
        res <- runBF content
        setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
            *> BS.putStr "Output:"
            *> setSGR [Reset] *> BS.putStr " "
            *> BS.putStrLn res

repl :: IO ()
repl = do
    BS.putStrLn "A little BrainFuck interpreter.\nType \":q\" to exit.\n"
    
    forever $ do
        hSetBuffering stdin LineBuffering
        
        c <- stripBS <$> (setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
            *> BS.putStr "Input some code:"
            *> setSGR [Reset] *> BS.putStr " "
            *> BS.getLine)
        
        if c == ":q"
           then exitSuccess
           else do
               hSetBuffering stdin NoBuffering
               res <- runBF c
               
               setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
                *> BS.putStr "Output:"
                *> setSGR [Reset] *> BS.putStr " "
                *> BS.putStrLn res

stripBS :: BS.ByteString -> BS.ByteString
stripBS = BS.dropWhile isSpace . fst . BS.spanEnd isSpace
