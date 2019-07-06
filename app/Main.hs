{-# LANGUAGE OverloadedStrings #-}

module Main where

import BF
import System.Exit
import Control.Monad
import System.Console.ANSI
import System.IO
import Data.ByteString.Char8 as BS
import Data.Char

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

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


stripBS :: ByteString -> ByteString
stripBS = BS.dropWhile isSpace . fst . BS.spanEnd isSpace