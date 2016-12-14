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

  
s1 = var "S_1"
m1 = var "M_1"
m2 = var "M_2"
m3 = var "M_3"

f :: String -> Float
f "S_1" = 0.5
f "M_1" = 0.5
f "M_2" = 0.5
f "M_3" = 0.5

v1 = s1 .& m1 .+ m2
v2 = s1 .+ m2 .+ m3
v3 = s1 .+ m3


exdraw e = drawPdfInfoSheet e ["S_1", "M_1", "M_2", "M_3"] (Just f) "prova.pdf"  
