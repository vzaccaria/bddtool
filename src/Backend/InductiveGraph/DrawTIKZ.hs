{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Backend.InductiveGraph.DrawTIKZ where

import           Backend.InductiveGraph.InductiveGraph
import           Control.Monad
import           Data.Graph.Inductive
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text.Lazy                        (unpack)
import           Utils.Plot                            (plotDot, plotTikz)

prologue = [i| \\begin{tikzpicture}[
>=latex, every node/.style={circle, draw, minimum size=0.75cm}]
\\graph [tree layout, level distance=0.5in, sibling distance=0.5in  ] {
|]

epilogue = [i|};
\\end{tikzpicture}|]

drawNodes :: BDD -> String
drawNodes b = let
  accumulateOnContext :: Context NodeLabel EdgeLabel -> String -> String
  accumulateOnContext c a = let

    nid = node' c
    nlb = lab' c

    edgesi = inn b nid
    edgeso = out b nid

    isroot = if length edgesi == 0 then "true" else "false"

    arrow (s,t,v) = let
      slab = fromMaybe "" $ lab b s
      tlab = fromMaybe "" $ lab b t
      arrstyle = if v then "" else "dashed"
      in [i|#{s}[root=#{isroot},as={#{slab}}] ->[#{arrstyle}] #{t}[as=#{tlab}]; |]

    arrows = concatMap arrow edgeso
    in
      [i|#{arrows} #{a}|]
  graph = ufold accumulateOnContext "" b
  in
    prologue ++ graph ++ epilogue

drawNodesAsPdf :: BDD -> String -> IO ()
drawNodesAsPdf b n = plotTikz n $ drawNodes b
