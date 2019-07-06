{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Data.Array
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Word
import           Control.Lens

data Memory = Memory { _memory :: Array Int Word8
                     , _cursor :: Int             }
makeLenses ''Memory

type Eval = StateT Memory (WriterT String IO)

data Instr = Next | Prev | Incr | Decr | Loop [Instr] | Get | Put | Null
    deriving (Eq, Show)

newtype Stream = Stream { input :: String }

type Parser = State Stream

cellsNumber :: Int
cellsNumber = 30000

initMemory :: Memory
initMemory = Memory
  { _memory = array (1, cellsNumber) (zip [1 ..] (replicate cellsNumber 0))
  , _cursor = 1
  }
