module Types where

import Data.Array
import Control.Monad.State
import Control.Monad.Writer

data Memory = Memory { memory :: Array Int Int
                     , cursor :: Int           }

type Eval = StateT Memory (Writer String)

data Instr = Next | Prev | Incr | Decr | Loop [Instr] | Get | Put | Null
    deriving (Eq, Show)

newtype Stream = Stream { input :: String }

type Parser = State Stream

cellsNumber :: Int
cellsNumber = 30000

initMemory :: Memory
initMemory = Memory { memory = array (1, cellsNumber) (zip [1..] (replicate cellsNumber 0))
                    , cursor = 1                                                            }