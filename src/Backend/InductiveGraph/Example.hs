
module Backend.InductiveGraph.Example where

import Language.Operators
import Backend.InductiveGraph.InductiveGraph


x = var "x" 
y = var "y"
z = var "z"

e0 = x .+ y .+ z .<> x .+ y .+ z .<> x .<> z
pe0 = pdotToFile "final.pdf" $ buildBDD ["x", "y", "z"] (unE e0)

v1 = unE $ neg x .+ y .& z .<> var "x" .<> ((neg x .+ y .+ z) .& x)
  
