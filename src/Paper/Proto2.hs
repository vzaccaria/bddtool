{-# LANGUAGE NoMonomorphismRestriction #-}
module Backend.InductiveGraph.Proto1 where

import           Backend.InductiveGraph.Draw
import           Backend.InductiveGraph.DrawTIKZ
import           Backend.InductiveGraph.InductiveGraph
import           Backend.InductiveGraph.PrintLatex
import           Backend.InductiveGraph.Probability
import           Data.Graph.Inductive.Graph
import           Language.Operators
import           Utils.Plot


-- 2, 5, 6, 9
  
s2 = var "S_2"
s5 = var "S_5"
s6 = var "S_6"
s9 = var "S_9"

m1 = var "M_1"
m2 = var "M_2"
m3 = var "M_3"
m4 = var "M_4"
m5 = var "M_5"
m6 = var "M_6"
m7 = var "M_7"
m8 = var "M_8"

f :: String -> Float
f "S_2"  = 0
f "S_5"  = 1
f "S_6"  = 1
f "S_9"  = 0
f _  = 0.5

v2 = s2 .+ m3 .+ m4
v5 = s5 .+ m1 .+ m4
v6 = s6 .+ m3 .+ m6
v9 = s9 .+ m1 .+ m6

vl         = ["S_2", "S_5", "S_6", "S_9", "M_3", "M_4", "M_1", "M_6"]


drawp vl e = drawPdfInfoSheet e vl (Just f)
drawnp e   = drawPdfInfoSheet e vl Nothing

dir        = "/Users/zaccaria/development/stforge/polimi-casca/docs/general-ideas/bdd/images"

writePdfs :: IO ()
writePdfs = do
  drawp vl (v2 .<> v5 .<> v6) (dir ++ "/tc_v2.pdf");

