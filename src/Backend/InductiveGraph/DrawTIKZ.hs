{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Backend.InductiveGraph.DrawTIKZ where

import           Backend.InductiveGraph.InductiveGraph
import           Backend.InductiveGraph.PrintLatex
import           Backend.InductiveGraph.Probability
import           Control.Monad
import           Data.Graph.Inductive
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text.Lazy                        (unpack)
import           Text.Printf
import           Utils.Plot                            (plotDot, plotTikz)

prologue = [i|
\\begin{tikzpicture}[>=latex, every node/.style={circle, draw, minimum size=0.55cm}]
\\graph [layered layout, level distance=0.5in, sibling distance=0.5in  ] {
|]

epilogue pfx = [i|};
#{pfx}
\\end{tikzpicture}
|]


rpos n (dx,dy) = [i|($(#{show n})+(#{dx},#{dy})$)|]

drawNodeRelativeTo :: Node -> (Float, Float) -> String -> String
drawNodeRelativeTo n d expl = [i| \\node [draw=none] at #{rpos n d} {#{expl}};|]


drawBarRelativeTo :: Node -> (Float, Float) -> Float -> Float -> Int -> Float -> String
drawBarRelativeTo n d dim dimy ix v =
  let
    (dx, dy) = d
    p   = (dx + (toEnum ix) * dim, dy)
    mw  = (printf "%.2fcm" dim) :: String
    hh  = (printf "%.2fcm" dimy) :: String
    mv  = (printf "%.2fcm" $ v * dimy) :: String
    sty = "rectangle, inner sep=0pt, draw=none, anchor=south, minimum width=" ++ mw
  in
    [i|\\node [#{sty}, fill=gray!30, minimum height=#{hh}] at #{rpos n p} {}; \n|] ++
    [i|\\node [#{sty}, fill=black,   minimum height=#{mv}] at #{rpos n p} {}; \n|]

data Dir = Lft | Rght

drawChart :: Node -> Dir -> [Float] -> String
drawChart n dr vs =
  let
    dimx = 0.05 :: Float
    dimy = 0.5 :: Float
    lvs :: Float
    lvs = toEnum $ length vs
    d = case dr of
      Rght -> (1, -0.5) :: (Float, Float)
      Lft -> (-0.2 - dimx * (lvs + 1), 0.2) :: (Float, Float)
    pairs = zip [1 .. length vs] vs
    res = concatMap (uncurry (drawBarRelativeTo n d dimx dimy)) pairs
  in
    res


drawNodes :: Maybe (String -> Float) -> BDD -> String -> String
drawNodes mf b expl = let

  addProb = case mf of
    Just f -> let
        dc (_, n, _, _) s = drawChart n Lft (nodeProb (toProbBDD f b) n) ++ s
      in
        ufold dc "" b
    Nothing -> ""

  pfx =
    drawNodeRelativeTo (findRootNode b) (0,1) expl ++ addProb

  accumulateOnContext :: Context NodeLabel EdgeLabel -> String -> String
  accumulateOnContext c a = let

    nid = node' c
    nlb = lab' c

    edgesi = inn b nid
    edgeso = out b nid

    isroot = if length edgesi == 0 then "true" else "false"

    lab'' b s = case lab b s of
      Just v -> show v
      Nothing -> ""

    arrow (s,t,v) = let
      slab = "$" ++ lab'' b s ++ "$"
      tlab = "$" ++ lab'' b t ++ "$"
      arrstyle = if v then "" else "dashed"
      in [i|#{s}[root=#{isroot},as={#{slab}},scale=0.5] ->[#{arrstyle}] #{t}[as=#{tlab},scale=0.5]; \n|]

    arrows = concatMap arrow edgeso
    in
      [i|#{arrows} #{a}|]
  graph = ufold accumulateOnContext "" b
  in
    prologue ++ graph ++ epilogue pfx


drawPdfInfoSheet e vs f n =
  let exp   = unE e
      expl  = printAsEquation exp
      bdd   = buildBDD vs exp
  in
      plotTikz n $ drawNodes f bdd expl
