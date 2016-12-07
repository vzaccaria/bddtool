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
  :: GraphvizParams Int String Bool () String
myParameters = nonClusteredParams {fmtNode = fn, fmtEdge = fe }
  where fn (_,l) = [toLabel l]
        fe (_,_,True) = []
        fe (_,_,False) = [styles [dashed]]

defaultVis :: BDD -> DotGraph Node
defaultVis = graphToDot myParameters

bddToPDF :: String -> BDD -> IO ()
bddToPDF n x =
  void $
  addExtension (runGraphviz (defaultVis x))
               Pdf 
               n
