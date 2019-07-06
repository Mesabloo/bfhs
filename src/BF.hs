{-# LANGUAGE LambdaCase #-}
module BF where

import           Types
import           Data.Array
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Char
import           System.IO.Unsafe
import           Data.Bifunctor
import           System.IO
import           Data.Word                      ( Word8 )
import           Control.Lens

runBF :: String -> IO String
runBF code =
  let parsed = parse code in execWriterT (evalStateT (eval parsed) initMemory)

-- Parsing part

parse :: String -> [Instr]
parse []       = []
parse (x : xs) = let (i, rest) = parseOne x xs in i : parse rest
 where
  parseOne :: Char -> String -> (Instr, String)
  parseOne x xs = case x of
    '+' -> (Incr, xs)
    '-' -> (Decr, xs)
    '>' -> (Next, xs)
    '<' -> (Prev, xs)
    ',' -> (Get, xs)
    '.' -> (Put, xs)
    '[' -> let (is, rest) = parseUntil xs ']' in (Loop is, rest)
    _   -> (Null, xs)

  parseUntil :: String -> Char -> ([Instr], String)
  parseUntil []       _ = ([], [])
  parseUntil (x : xs) i = if x == i
    then ([], xs)
    else let (i', stream) = parseOne x xs in first (i' :) (parseUntil stream i)

cell :: Lens' Memory Word8
cell = lens getter setter
 where
  getter :: Memory -> Word8
  getter st =
    let mem = st ^. memory
        idx = st ^. cursor
    in  mem ! idx
  setter :: Memory -> Word8 -> Memory
  setter st val =
    let idx = st ^. cursor in st & memory %~ (\mem -> mem // [(idx, val)])

-- Evaluation part

eval :: [Instr] -> Eval ()
eval = mapM_ evalOne

evalOne :: Instr -> Eval ()
evalOne Next            = cursor += 1
evalOne Prev            = cursor -= 1
evalOne Incr            = cell += 1
evalOne Decr            = cell -= 1
evalOne i@(Loop instrs) = use cell >>= \case
  0 -> pure ()
  _ -> eval instrs *> evalOne i
evalOne Get = do
  c <- toEnum . ord <$> liftIO
    (putStr "Input character: " *> hFlush stdout *> getChar <* putStrLn "")
  cell .= c
evalOne Put = do
  val <- use cell
  lift $ tell [chr $ fromEnum val]
evalOne Null = pure ()
