{-# LANGUAGE QuasiQuotes #-}
module Utils.Plot where

import System.Process
import System.Directory
import Data.String.Interpolate

plotDot :: String -> String -> IO ()
plotDot name exp = do
    system "rm -rf .bdd";
    createDirectory ".bdd";
    writeFile ".bdd/bdd.dot" $ exp;
    system ("dot -Tpdf .bdd/bdd.dot -o .bdd/prefinal.pdf");
    system ("pdfcrop .bdd/prefinal.pdf " ++ name);
    return ();

tikzPrologue = [i|
\\RequirePackage{luatex85}
\\documentclass{standalone}
\\usepackage{tikz}
\\usetikzlibrary{graphdrawing}
\\usetikzlibrary{graphs}
\\usetikzlibrary{mindmap}
\\usetikzlibrary{positioning}
\\usetikzlibrary{snakes}
\\usetikzlibrary{arrows.meta}
\\usetikzlibrary{shadows}
\\usetikzlibrary{calc}
\\usetikzlibrary{fit}
\\usetikzlibrary{chains}
\\usetikzlibrary{quotes}
\\usetikzlibrary{shapes}
\\usetikzlibrary{matrix}
\\usetikzlibrary{positioning}
\\usetikzlibrary{decorations,decorations.pathmorphing,decorations.pathreplacing}
\\usegdlibrary{trees}
\\usegdlibrary{layered}

\\begin{document}
|]

tikzEpilogue = [i|
\\end{document}
|]

plotTikz :: String -> String -> IO ()
plotTikz name exp =
  let fileContents = tikzPrologue ++ exp ++ tikzEpilogue in
    do
      system "rm -rf .tikzgraph";
      createDirectory ".tikzgraph";
      writeFile ".tikzgraph/graph.tex" $ fileContents;
      system ("cd .tikzgraph && lualatex -shell-escape -interaction nonstopmode graph.tex")
      system ("pdfcrop .tikzgraph/graph.pdf " ++ name);
      return ();
