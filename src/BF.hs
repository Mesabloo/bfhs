module BF where

import Types
import Data.Array
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import System.IO.Unsafe
import Data.Bifunctor

runBF :: String -> String
runBF code =
    let parsed = parse code
    in execWriter (evalStateT (eval parsed) initMemory)

-- Parsing part
  
parse :: String -> [Instr]
parse   []   = []
parse (x:xs) = 
    let (i, rest) = parseOne x xs
    in i : parse rest
  where parseOne :: Char -> String -> (Instr, String)
        parseOne x xs = case x of
            '+' -> (Incr, xs)
            '-' -> (Decr, xs)
            '>' -> (Next, xs)
            '<' -> (Prev, xs)
            ',' -> (Get, xs)
            '.' -> (Put, xs)
            '[' ->
                let (is, rest) = parseUntil xs ']'
                in (Loop is, rest) 
            _   -> (Null, xs)
    
        parseUntil :: String -> Char -> ([Instr], String)
        parseUntil   []   _ = ([], [])
        parseUntil (x:xs) i =
            if x == i
            then ([], xs)
            else
                let (i', stream) = parseOne x xs
                in first (i':) (parseUntil stream i)
    
-- Evaluation part

eval :: [Instr] -> Eval ()
eval = mapM_ evalOne

evalOne :: Instr -> Eval ()
evalOne Next = modify $ \st -> st { cursor = let val = cursor st in if val == cellsNumber then 1 else val + 1 }
evalOne Prev = modify $ \st -> st { cursor = let val = cursor st in if val == 1 then cellsNumber else val - 1 }
evalOne Incr = modify $ \st -> st { memory = let idx = cursor st
                                                 val = memory st ! idx
                                             in memory st // [(idx, val + 1)] }
evalOne Decr = modify $ \st -> st { memory = let idx = cursor st
                                                 val = memory st ! idx
                                             in memory st // [(idx, val - 1)] }
evalOne i@(Loop instrs) =
    eval instrs
    *> do
        mem <- gets memory
        idx <- gets cursor
        
        case mem ! idx of
            0 -> pure ()
            _ -> evalOne i
evalOne Get = do
    let c = unsafePerformIO $ ord <$> (putStr "Input character: " *> liftIO getChar)
    modify $ \st -> st { memory = let idx = cursor st
                                  in memory st // [(idx, c)] }
evalOne Put = do
    mem <- gets memory
    idx <- gets cursor
    
    lift $ tell [chr (mem ! idx)]
evalOne Null = pure ()