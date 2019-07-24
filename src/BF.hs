{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module BF where

import           Types
import           Data.Array
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Char
import           Data.Bifunctor
import           System.IO
import           Data.Word                      ( Word8 )
import           Control.Lens
import           Data.ByteString.Char8 as BS



runBF :: ByteString -> IO ByteString
runBF code =
  let parsed = parse code in execWriterT (evalStateT (eval parsed) initMemory)
{-# INLINE runBF #-}

-- Parsing part

parse :: ByteString -> [Instr]
parse bs | BS.null bs = []
         | otherwise  = let (i, rest) = parseOne (BS.head bs) (BS.tail bs) in i : parse rest
 where
  parseOne :: Char -> ByteString -> (Instr, ByteString)
  parseOne x xs = case x of
    '+' -> (Incr, xs)
    '-' -> (Decr, xs)
    '>' -> (Next, xs)
    '<' -> (Prev, xs)
    ',' -> (Get, xs)
    '.' -> (Put, xs)
    '[' -> let (is, rest) = parseUntil xs ']' in (Loop is, rest)
    _   -> (Null, xs)
  {-# INLINABLE parseOne #-}

  parseUntil :: ByteString -> Char -> ([Instr], ByteString)
  parseUntil bs i | BS.null bs = ([], BS.empty)
                  | otherwise  =
                        let x  = BS.head bs
                            xs = BS.tail bs
                        in if x == i
                           then ([], xs)
                           else let (i', stream) = parseOne x xs in first (i' :) (parseUntil stream i)
  {-# INLINABLE parseUntil #-}
{-# INLINABLE parse #-}

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
{-# INLINE eval #-}

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
    (BS.putStr "Input character: " *> hFlush stdout *> getChar <* BS.putStrLn "")
  cell .= c
evalOne Put = do
  val <- use cell
  lift . tell $ BS.cons (chr $ fromEnum val) BS.empty
evalOne Null = pure ()
{-# INLINE evalOne #-}
