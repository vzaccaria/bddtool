{-# LANGUAGE DataKinds #-}
module Paper.Fourier where

import Control.Monad
import Language.Operators
import Backend.UnitInterval.UnitInterval
import Data.List

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

chi s x = foldl (.+) bot  $ map (x !!) s

b 0 = [[bot], [top]]
b n = [ x ++ y | x <- (b 0), y <- b (n - 1)]


cf f s x = unB (f x) * unB (chi s x)

fourier f n = let
  bs     = b (n - 1)
  pset   = powerset [0 .. (n - 1)]
  in do
    s  <- pset;
    return $ (s, sum (map (cf f s) bs)/(2.0 ** fromIntegral n))

ff [x, y, z] = x .& y .& z
xx [x, y, z] = x .+ y .+ z

q = fourier xx 3
