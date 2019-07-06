{-# LANGUAGE BlockArguments #-}

module Main where

import BF
import System.Exit
import Data.String.Utils
import Control.Monad
import System.IO
import System.Console.ANSI

main :: IO ()
main = do
     putStrLn "A little BrainFuck interpreter.\nType “:q” to exit.\n"
     
     forever $ do
          c <- setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
               *> putStr "Input some code:"
               *> setSGR [Reset] *> putStr " " *> hFlush stdout
               *> getLine

          if strip c == ":q"
          then exitSuccess
          else setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
               *> putStr "Output:"
               *> setSGR [Reset] *> putStr " "
               *> putStr (runBF $ strip c) *> hFlush stdout
