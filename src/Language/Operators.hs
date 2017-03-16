{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Language.Operators where

import GHC.TypeLits (type (+), Nat)

-- For operator precedence see here:
-- http://math.stackexchange.com/questions/128504/boolean-algebra-operation-precedence
infixr 8 .&

infixr 7 .|

infixr 5 .+

infixl 3 .<>

data V (a :: Nat) =
  V ()

class Symantics r  where
  h :: r (V 1) -> Double
  var :: String -> r (V 1)
  top :: r (V 1)
  bot :: r (V 1)
  neg :: r (V 1) -> r (V 1)
  (.&) :: r (V 1) -> r (V 1) -> r (V 1)
  (.|) :: r (V 1) -> r (V 1) -> r (V 1)
  (.+) :: r (V 1) -> r (V 1) -> r (V 1)
  (.<>) :: r (V n) -> r (V 1) -> r (V (n + 1))
