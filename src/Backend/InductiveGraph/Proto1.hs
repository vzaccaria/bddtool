{-# LANGUAGE NoMonomorphismRestriction #-}

module Backend.InductiveGraph.Proto1 where

import Backend.InductiveGraph.Draw
import Backend.InductiveGraph.DrawTIKZ
import Backend.InductiveGraph.InductiveGraph
import Backend.InductiveGraph.PrintLatex
import Backend.InductiveGraph.Probability
import Data.Graph.Inductive.Graph
import Language.Operators
import Utils.Plot

s1 = var "S_1"

m1 = var "S_2"

m2 = var "S_3"

m3 = var "S_4"

f :: String -> Float
f "S_1" = 0.5
f "S_2" = 0.5
f "S_3" = 0.5
f "S_4" = 0.5

v1 = s1 .& m1 .+ m2

v2 = s1 .+ m2 .+ m3

v3 = s1 .+ m3

vl = ["S_1", "S_2", "S_3", "S_4"]

vl123 = ["S_1", "S_2", "S_3", "S_4"]

vl23 = ["S_1", "S_3", "S_4"]

vl3 = ["S_1", "S_4"]

drawp vl e = drawPdfInfoSheet e vl (Just f)

drawnp e = drawPdfInfoSheet e vl Nothing

dir =
  "/Users/zaccaria/development/github/org-crypto/polimi-casca/docs/general-ideas/bdd/images"

writePdfs :: IO ()
writePdfs = do
  drawnp (s1 .| m1) (dir ++ "/s1_p_m1.pdf")
  drawnp (s1 .+ m1) (dir ++ "/s1_x_m1.pdf")
  drawnp (s1 .& m1 .+ m2) (dir ++ "/s1_t_m1_x_m2.pdf")
  drawnp (s1 .& m1) (dir ++ "/s1_t_m1.pdf")
  drawp vl (s1 .& m1) (dir ++ "/s1_t_m1_r.pdf")
  drawp vl (s1 .+ m1) (dir ++ "/s1_x_m1_r.pdf")
  drawp vl (s1 .| m1) (dir ++ "/s1_p_m1_r.pdf")
  drawp vl (v1) (dir ++ "/v1_r.pdf")
  drawp vl23 (v2) (dir ++ "/v2_r.pdf")
  drawp vl3 (v3) (dir ++ "/v3_r.pdf")
  drawp vl (v1 .<> v2) (dir ++ "/v1_m_v2_r.pdf")
  drawp vl (v2 .<> v3) (dir ++ "/v2_m_v3_r.pdf")
  drawp vl (v1 .<> v3) (dir ++ "/v1_m_v3_r.pdf")
  drawp vl (v1 .<> v2 .<> v3) (dir ++ "/v1_m_v2_m_v3_r.pdf")
  drawp vl (s1 .& m1 .+ m1) (dir ++ "/s1_t_m1_x_m1_r.pdf")
  drawp vl (s1 .& m1 .| m1) (dir ++ "/s1_t_m1_p_m1_r.pdf")
