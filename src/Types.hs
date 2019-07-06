module Types where

import Data.Array
import Control.Monad.State
import Control.Monad.Writer
import Data.Word

data Memory = Memory { memory :: Array Word16 Word8
                     , cursor :: Word16             }

type Eval = StateT Memory (WriterT String IO)

data Instr = Next | Prev | Incr | Decr | Loop [Instr] | Get | Put | Null
    deriving (Eq, Show)

initMemory :: Memory
initMemory = Memory { memory = array (minBound, maxBound) (zip [minBound..] (replicate maxBound minBound))
                    , cursor = minBound                                                                       }