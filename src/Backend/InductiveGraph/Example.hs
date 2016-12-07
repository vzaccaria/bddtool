module Backend.InductiveGraph.Example where

import Backend.InductiveGraph.Draw
import Backend.InductiveGraph.InductiveGraph
import Language.Operators

x = var "x"

y = var "y"

z = var "z"

e0 = x .+ y .<> x .<> z
  
pe0 =
  bddToPDF "finalGV" $
  buildBDD ["x","y","z"]
           (unE e0)

v1 = unE $ neg x .+ y .& z .<> var "x" .<> ((neg x .+ y .+ z) .& x)

main = pe0
