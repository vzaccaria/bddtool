{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Backend.UnitInterval.UnitInterval where

import           Data.Maybe               ()
import           Language.Operators

-- B is a type function that will collapse to our internal expression type
-- We are going to make E an instance of our symantics
data family   B a
data instance B (V n) = B{unB :: Double}

instance Symantics B where
  bot = B 1.0
  top = B $ -1.0
  neg x = B (-1 * unB x)
  x .& y =
    let x' = unB x
        y' = unB y
    in B $ 0.5 + 0.5 * x' + 0.5 * y' - 0.5 * x' * y'
  x .+ y = B (unB x * unB y)
  var _ = error "!"
  _ .| _ = error "!"
  _ .<> _ = error "!"




