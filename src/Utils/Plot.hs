
module Utils.Plot where

import System.Process
import System.Directory

plotDot :: String -> String -> IO ()
plotDot name exp = do
    system "rm -rf .bdd";
    createDirectory ".bdd";
    writeFile ".bdd/bdd.dot" $ exp;
    system ("dot -Tpdf .bdd/bdd.dot -o .bdd/prefinal.pdf");
    system ("pdfcrop .bdd/prefinal.pdf " ++ name);
    return ();
