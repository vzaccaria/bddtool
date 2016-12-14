{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend.InductiveGraph.Probability (toProbBDD, nodeProb) where

import           Backend.InductiveGraph.InductiveGraph
import           Control.Arrow                         (first)
import           Control.Monad
import           Data.Graph.Inductive
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text.Lazy                        (unpack)
import           Utils.Plot                            (plotDot, plotTikz)

type ExtendedNodeLabel = (NodeLabel, Float, [Float])
type ProbBDD           = Gr ExtendedNodeLabel EdgeLabel

boolToFloat :: Fractional b => [Bool] -> [b]
boolToFloat bv = let
  as = 2 ^ (length bv)
  iv = boolToInt bv
  f i = if i == iv then 1.0 else 0.0 
  rv = [ f i | i <- [0 .. as - 1] ]
 in
  rv

boolToInt :: [Bool] -> Int
boolToInt bv =
  let
    bs = map (\x -> if x then 1 else 0) bv
    bb = reverse [ 2^i | i <- [ 0 .. (length bv) - 1]]
    zp = sum $ zipWith (*) bb bs 
  in
    zp


groundprob :: ProbBDD -> Node -> [Float]
groundprob b n = case (context b n) of
  (_, _, (D _, _, gp), _) -> gp
  _ -> error "cant compute ground prob"

toProbBDD :: (String -> Float) -> BDD -> ProbBDD
toProbBDD f b =
  propagate $ annote f b

annote :: (String -> Float) -> BDD -> ProbBDD
annote f b =
  let
    g nl@(Vr s) = (nl, f s, [])
    g nl@(D bv) = (nl, 1.0, boolToFloat bv)
  in
    nmap g b

propagate :: ProbBDD -> ProbBDD
propagate b =
  let
    f (r, n, (nl, pr, pv), s) = (r, n, (nl, pr, nodeProb b n), s)
  in
    gmap f b

isLeaf b n = case (context b n) of
  (_, _, (D _,_,_), _) -> True
  _ -> False

branchProb b n = case (context b n) of
  (_,_, (Vr _, bp, _), _) -> bp
  _ -> error "sorry, this is not a branch node"

nodeProb :: ProbBDD -> Node -> [Float]
nodeProb b n =
  if isLeaf b n
  then
    groundprob b n
  else
    let (nf, nt) = subNodes b n
        (gf, gt) = (nodeProb b nf, nodeProb b nt)
        p = branchProb b n
        s = zipWith (+) (map ((1-p) *) gf) (map (p *) gt)
    in
      s

subNodes :: ProbBDD -> Node -> (Node, Node)
subNodes b n =
  let
    (_, _, nl, s) = context b n
  in
    case s of
         [(False, nf), (True, nt)] -> (nf, nt)
         [(True, nt), (False, nf)] -> (nf, nt)
         _ -> error [i| node (#{show n} - #{nl}) found to have outgoing edges #{show s} |]






