{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Paper.Proto2 where

import           Backend.InductiveGraph.Draw
import           Backend.InductiveGraph.DrawTIKZ
import           Backend.InductiveGraph.InductiveGraph 
import           Backend.InductiveGraph.PrintLatex 
import           Backend.InductiveGraph.Probability
import           Control.Lens 
import           Language.Operators
import           Utils.Plot


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
m9 = var "M_9"

f :: String -> Float
f "S_2"  = 0
f "S_5"  = 1
f "S_6"  = 1
f "S_9"  = 0
f _  = 0.5

v2  = s2 .+ m3 .+ m4
v5  = m1 .+ m4
v6  = m3 .+ m6
v9  = m1 .+ m6
v10 = m1 .+ m7
v11 = m1 .+ m6 .+ m7
vl  = ["S_2", "M_3", "M_4", "M_1", "M_6"]
vl3 = ["S_2", "M_3", "M_4"]
vl9 = vl ++ ["M_9"]
vl10 = ["S_2", "M_7", "M_6", "M_3", "M_4", "M_1"]

plotProbAllVariables vl=
  set drawArrows True .
  set probabilities (Just f) .
  set variablesList vl
  $ defaultOptions

drawp vl = drawPdfInfoSheet (plotProbAllVariables vl) 

dir        = "/Users/zaccaria/development/stforge/polimi-casca/docs/general-ideas/bdd/images"

writePdfs :: IO ()
writePdfs = do
  -- drawp vl (v2 .<> v5 .<> v6 .<> v9) (dir ++ "/tc_v2569.pdf");
  -- drawp vl3 (v2 .<> m3 .<> m4) (dir ++ "/tc_v2_m34.pdf");
  -- drawp vl9 (v2 .+ m9 .<> v5 .+ m9  .<> v6 .+ m9  .<> v9 .+ m9 ) (dir ++ "/tc_v2569_m9.pdf");
  -- drawp vl10 (v2 .<> v5 .<> v6 .<> v10) (dir ++ "/tc_v25610.pdf");
  drawp vl10 (v2 .<> v5 .<> v6 .<> v11) (dir ++ "/tc_v25611.pdf");

