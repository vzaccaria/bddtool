module Backend.InductiveGraph.Example where

import           Backend.InductiveGraph.Draw
import           Backend.InductiveGraph.DrawTIKZ
import           Backend.InductiveGraph.InductiveGraph
import Utils.Plot
import           Data.Graph.Inductive.Graph
import           Language.Operators


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
bddFrom e = buildBDD ["x","y","z"] (unE e)

v1 = unE $ neg x .+ y .& z .<> var "x" .<> ((neg x .+ y .+ z) .& x)
 
main = pe0


tikzex = putStrLn $ drawNodes $ bddFrom e0
