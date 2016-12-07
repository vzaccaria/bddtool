{-# LANGUAGE OverloadedStrings #-}

module Backend.InductiveGraph.Draw where

import           Backend.InductiveGraph.InductiveGraph
import           Control.Monad
import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.GraphViz.Printing
import           Data.Text.Lazy                        (unpack)
import           Utils.Plot                            (plotDot)

myParameters
  :: GraphvizParams Int String () () String
myParameters = nonClusteredParams {fmtNode = fn}
  where fn (_,l) = [toLabel l]

defaultVis :: BDD -> DotGraph Node
defaultVis = graphToDot myParameters

bddToPDF :: String -> BDD -> IO ()
bddToPDF n x =
  void $
  addExtension (runGraphviz (defaultVis x))
               Pdf 
               n
