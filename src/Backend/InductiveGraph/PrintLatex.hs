{-# LANGUAGE QuasiQuotes #-}
module Backend.InductiveGraph.PrintLatex where


import Backend.InductiveGraph.InductiveGraph
import Data.String.Interpolate

p :: String -> String
p s = "(" ++ s ++ ")"

printAsEquation :: Exp -> String
printAsEquation e = [i|
$#{prettyPrintLatex e}$
|]

prettyPrintLatex :: Exp -> String
prettyPrintLatex (EVAR s    )  = s
prettyPrintLatex (EAND e1 e2)  = pwp 8 e1 ++ "  " ++ pwp 8 e2
prettyPrintLatex (EOR  e1 e2)  = pwp 7 e1 ++ " \\vee "   ++ pwp 7 e2
prettyPrintLatex (EXOR e1 e2)  = pwp 5 e1 ++ " \\oplus " ++ pwp 5 e2
prettyPrintLatex (ENEG e1)     = "\\neg " ++ pwp 10 e1
prettyPrintLatex (EMELD e1 e2) = pwp 3 e1 ++ "~~\\diamond~~" ++ pwp 3 e2

prec (ENEG _)    = 10
prec (EVAR s)    = 10
prec (EAND _ _)  = 8
prec (EOR _ _ )  = 7
prec (EXOR _ _)  = 5
prec (EMELD _ _) = 3

pwp curPrec exp =
  if curPrec > prec exp then (p . prettyPrintLatex) exp else prettyPrintLatex exp
