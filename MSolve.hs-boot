module MSolve where

import {-# SOURCE #-} MPoly

msolve :: (Show coeff, Real coeff, Fractional coeff) => [MPoly coeff] -> [[[coeff]]]
