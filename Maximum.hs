module Maximum where

import qualified Prelude
import GHC.Stack
import Prelude (null, error, Ord)

maximum :: (HasCallStack, Ord a) => [a] -> a
maximum xs = if null xs then error "maximum [] not computable" else Prelude.maximum xs