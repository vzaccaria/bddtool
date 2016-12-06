{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Backend.InductiveGraph where

import qualified Data.Graph                   as G
import           Data.Graph.Inductive
import           Data.Graph.Inductive.Example

import qualified Data.GraphViz                as DG
import qualified Data.GraphViz.Printing       as DGP
import qualified Data.Map                     as M

import           Data.Text.Lazy                    (unpack)
import Language.Operators

-- Ideally, this should be done in fgl
-- data ITE = ITE String ITE ITE | TERM [Bool]
-- Where each node is labelled with an expression

-- E is a type function that will collapse to our internal expression type
-- We are going to make E an instance of our symantics

data family E a
data instance E (V n) = E { unE::Exp }

data Exp = EVAR String | EAND Exp Exp | EOR Exp Exp | EXOR Exp Exp | ENEG Exp | EMELD Exp Exp deriving (Show)

instance Symantics E where
  var     = E . EVAR
  neg     = E . ENEG . unE
  x .& y  = E  (unE x `EAND` unE y) 
  x .+ y  = E  (unE x `EXOR` unE y)
  x .| y  = E  (unE x `EOR` unE y)
  f .<> e = E  (unE f `EMELD` unE e)

x = var "x"
y = var "y"
z = var "z"

v1 = neg x .+ y .& z .<> var "x" .<> ((neg x .+ y .+ z) .& x)


-- import           OBDD.Melt.Operator
-- import           OBDD.Melt.Paper
-- import OBDD.Graphics.PDF

-- convertToList :: Mop -> [ (MopValue, MopKey, [MopKey])]
-- convertToList (m,root) | (Just (F x)) <- M.lookup root m = [(F x, root, [])]
-- convertToList (m,root) | (Just (N (x,k1,k2))) <- M.lookup root m =
--                               [(N (x,k1,k2), root, [k1,k2])] ++ convertToList (m, k1) ++ convertToList (m, k2)

-- convertToGraph :: Mop -> G.Graph
-- convertToGraph v = let (g, _, _) = G.graphFromEdges $ convertToList v in g

-- -- Unlabeled edges and integer labeled nodes
-- toInductiveGraph :: G.Graph -> Gr () ()
-- toInductiveGraph g = let
--   es = G.edges g
--   vs = G.vertices g
--   unl = zip vs (repeat ())
--   lue = labUEdges es
--   in mkGraph unl lue

-- mopToIG :: Mop -> Gr () ()
-- mopToIG = toInductiveGraph . convertToGraph

-- -- instance DG.Labellable () where
-- --   toLabelValue x = Label $ StrLabel ""

-- gv2 :: Gr () ()
-- gv2 = mopToIG v2

-- pdot :: Mop -> String
-- pdot x = unpack . DGP.renderDot . DGP.toDot . DG.graphToDot DG.nonClusteredParams $ mopToIG x

-- pdotFinal :: Mop -> IO ()
-- pdotFinal x =  plotDot "final.pdf" $ pdot x
