{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Data.Array
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Word
import           Control.Lens
import           Data.ByteString.Char8 hiding (replicate, zip)

data Memory = Memory { _memory :: Array Word16 Word8
                     , _cursor :: Word16             }
makeLenses ''Memory

type Eval = StateT Memory (WriterT ByteString IO)

data Instr = Next | Prev | Incr | Decr | Loop [Instr] | Get | Put | Null
    deriving (Eq, Show)


initMemory :: Memory
initMemory = Memory
  { _memory = array (minBound, maxBound)
                    (zip [minBound ..] (replicate maxBound minBound))
  , _cursor = minBound
  }
