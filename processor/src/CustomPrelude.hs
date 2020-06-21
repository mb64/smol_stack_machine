module CustomPrelude (module Clash.Prelude, Dom, WordBits) where

import Clash.Prelude hiding (blockRam)

-- | Processor clock domain
--
-- For now, it's just the system clock domain
type Dom = System

-- | The processor's word type
type WordBits = BitVector 8
