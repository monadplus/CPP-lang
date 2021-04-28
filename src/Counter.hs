module Counter (Counter, newCounter, nextCounter) where

import Data.Word

newtype Counter = Counter Word64

newCounter :: Counter
newCounter = Counter 0

nextCounter :: Counter -> (Counter, Word64)
nextCounter (Counter c) =
  let !c' = c + 1
   in (Counter c', c)
