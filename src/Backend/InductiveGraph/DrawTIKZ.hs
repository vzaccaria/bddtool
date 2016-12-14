{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Backend.InductiveGraph.DrawTIKZ where

import           Backend.InductiveGraph.InductiveGraph
import           Backend.InductiveGraph.PrintLatex
import           Control.Monad
import           Data.Graph.Inductive
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text.Lazy                        (unpack)
import           Utils.Plot                            (plotDot, plotTikz)

prologue = [i|
\\begin{tikzpicture}[>=latex, every node/.style={circle, draw, minimum size=0.75cm}]
\\graph [layered layout, level distance=0.5in, sibling distance=0.5in  ] {
|]

epilogue pfx = [i|};
#{pfx}
\\end{tikzpicture}
|]


rpos n (dx,dy) = [i|($(#{show n})+(#{dx},#{dy})$)|]

drawNodeRelativeTo :: Node -> (Float, Float) -> String -> String
drawNodeRelativeTo n d expl = [i| \\node [draw=none] at #{rpos n d} {#{expl}};|]


drawBarRelativeTo :: Node -> (Float, Float) -> Float -> Int -> Float -> String
drawBarRelativeTo n d dim ix v =
  let
    (dx, dy) = d
    p = (dx + (toEnum ix) * dim, dy)
  in
    [i| \\node [rectangle, inner sep=0pt, draw=none, fill=gray!30, minimum width=#{show dim}cm, minimum height=1cm , anchor=south] at #{rpos n p} {};|] ++
    [i| \\node [rectangle, inner sep=0pt, draw=none, fill=black, minimum size=0cm, minimum width=#{show dim}cm, minimum height=#{show v}cm, anchor=south] at #{rpos n p} {};|] 

data Dir = Lft | Rght

drawChart :: Node -> Dir -> [Float] -> String
drawChart n dr vs = 
  let
    dim :: Float
    dim = 0.2
    lvs :: Float
    lvs = toEnum $ length vs
    d = case dr of
      Rght -> (1, -0.5) :: (Float, Float)
      Lft -> (-1 - dim * (lvs + 1), -0.5) :: (Float, Float)
    pairs = zip [1 .. length vs] vs
    res = concatMap (uncurry (drawBarRelativeTo n d dim)) pairs
  in
    res

findRootNode :: BDD -> Node
findRootNode b = let
    nds = nodes b
    rts = filter (\n -> (length (inn b n) == 0)) nds
  in
    head rts

drawNodes :: BDD -> String -> String
drawNodes b expl = let

  pfx =
    drawNodeRelativeTo (findRootNode b) (0,1) expl ++
    drawChart (findRootNode b) Rght [ 0.1, 0.3, 0.5, 0.0 ] ++ 
    drawChart (findRootNode b) Lft [ 0.1, 0.3, 0.5, 0.0 ] 

  accumulateOnContext :: Context NodeLabel EdgeLabel -> String -> String
  accumulateOnContext c a = let

    nid = node' c
    nlb = lab' c

    edgesi = inn b nid
    edgeso = out b nid

    isroot = if length edgesi == 0 then "true" else "false"

    arrow (s,t,v) = let
      slab = "$" ++ (fromMaybe "" $ lab b s) ++ "$"
      tlab = "$" ++ (fromMaybe "" $ lab b t) ++ "$"
      arrstyle = if v then "" else "dashed"
      in [i|#{s}[root=#{isroot},as={#{slab}}] ->[#{arrstyle}] #{t}[as=#{tlab}]; |]

    arrows = concatMap arrow edgeso
    in
      [i|#{arrows} #{a}|]
  graph = ufold accumulateOnContext "" b
  in
    prologue ++ graph ++ epilogue pfx

drawNodesAsPdf :: BDD -> String -> IO ()
drawNodesAsPdf b n = plotTikz n $ drawNodes b ""

drawPdfInfoSheet e vs n =
  let exp = unE e    
      expl = printAsEquation exp
      nexpl = expl
      bdd = buildBDD vs exp
  in
      plotTikz n $ drawNodes bdd nexpl
