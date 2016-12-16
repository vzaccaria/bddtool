module Backend.InductiveGraph.Example where

import Backend.InductiveGraph.Draw
import Backend.InductiveGraph.DrawTIKZ
import Backend.InductiveGraph.InductiveGraph
import Backend.InductiveGraph.PrintLatex
import Backend.InductiveGraph.Probability
import Data.Graph.Inductive.Graph
import Language.Operators
import Utils.Plot
import Control.Lens

x = var "x"
y = var "y"
z = var "z"

e0 = x .+ y

be0 :: BDD
be0 = bddFrom e0

pe0 =
  bddToPDF "finalGV" $
  buildBDD ["x","y","z"]
           (unE e0)

bddFrom :: E (V n) -> BDD
bddFrom e =
  buildBDD ["x","y","z"]
           (unE e)

v1 = neg x .+ y .& z .<> var "x" .<> ((neg x .+ y .+ z) .& x)

main = pe0

plotSimple =
  set probabilities (Just $ const 0.5) .
  set variablesList ["x", "y", "z"]
  $ defaultOptions



drawp e = drawPdfInfoSheet plotSimple e
exdraw e = drawp e "prova.pdf"
