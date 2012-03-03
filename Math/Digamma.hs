module Math.Digamma (digamma) where

import Math.Polynomial

-- | A robust implementation of the digamma function
-- This code is based on the implementation by Tom Minka
digamma :: RealFloat a => a -> a
digamma 0 = -1/0 -- Should we bail here?
digamma x | x < 0 = let x' = -1*x in digamma (x'+1) + pi / tan (pi*x') -- Use reflection formula
digamma 1 = -0.5772156649015328606065121 -- Euler-Mascheroni constant
digamma 2 = pi**2 / 6
digamma x | x <= small = digamma 1 - 1/x + digamma 2 * x
          -- Reduce to digamma(x+n) where (x+n) >= large
          | x > small && x < large = digamma (x+1) - 1/x -- TODO
  where small = 1e-6
        large = 9.5
digamma x = let r = 1/x
            in log x -  p `evalPoly` (1/x)
  where p = poly LE [ 0, 1/2, 1/12, 1/120, 1/252, 1/240, 1/132, 691/32760, 1/12, 3617/8160 ]
{-# SPECIALIZE digamma :: Double -> Double #-}
{-# SPECIALIZE digamma :: Float -> Float #-}

