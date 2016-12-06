{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Backend.InductiveGraph where

import           Data.Graph.Inductive
import Data.List
import Data.Maybe
import Data.Bool (Bool(..))
import qualified Data.GraphViz                as DG
import qualified Data.GraphViz.Printing       as DGP
import           Data.Text.Lazy                    (unpack)
import Language.Operators
import Utils.Plot (plotDot)
import Control.Monad.State.Lazy hiding (lift)
import Control.Monad (unless, void)

-- Ideally, this should be done in fgl
-- data ITE = ITE String ITE ITE | TERM [Bool]
-- Where each node is labelled with an expression

-- E is a type function that will collapse to our internal expression type
-- We are going to make E an instance of our symantics

data family E a
data instance E (V n) = E { unE::Exp }

data Exp = ETRUE | EFALSE | EVAR String | EAND Exp Exp | EOR Exp Exp | EXOR Exp Exp | ENEG Exp | EMELD Exp Exp deriving (Show,Eq)

instance Symantics E where
  var     = E . EVAR
  neg     = E . ENEG . unE
  x .& y  = E  (unE x `EAND` unE y) 
  x .+ y  = E  (unE x `EXOR` unE y)
  x .| y  = E  (unE x `EOR` unE y)
  f .<> e = E  (unE f `EMELD` unE e)

partialEval :: (Maybe (String, Exp)) -> Exp -> Exp
partialEval Nothing e                  = e
partialEval _ ETRUE                    = ETRUE
partialEval _ EFALSE                   = EFALSE
partialEval (Just (vr, vv)) a@(EVAR s) = if s == vr then vv else a
partialEval c (EAND e1 e2)             = EAND (partialEval c e1) (partialEval c e2)
partialEval c (EOR e1 e2)              = EOR (partialEval c e1) (partialEval c e2)
partialEval c (EXOR e1 e2)             = EXOR (partialEval c e1) (partialEval c e2)
partialEval c (ENEG e1)                = ENEG (partialEval c e1)
partialEval c (EMELD e1 e2)            = EMELD (partialEval c e1) (partialEval c e2)

eval :: Exp -> [Bool]
eval (EVAR s)     = error ("Sorry, " ++ show s ++ " cant be fully evaluated")
eval ETRUE        = [True]
eval EFALSE       = [False]
eval (EAND e1 e2) = [(head (eval e1)) && (head (eval e2))]
eval (EOR e1 e2)  = [(head (eval e1)) || (head (eval e2))]

x = var "x"
y = var "y"
z = var "z"

v1 = unE $ neg x .+ y .& z .<> var "x" .<> ((neg x .+ y .+ z) .& x)

data InternalFun = ITE String InternalFun InternalFun | TERM [Bool] deriving (Eq,Show)
type NodeLabel = String
type EdgeLabel = ()

type BDD = Gr NodeLabel EdgeLabel
type BDDState a =  State BDD a

getFreshNumber _ = 1

mk :: String -> Node -> Node -> BDDState Node
mk l v0 v1 = do {
  b <- get;
  case duplicateExists b l v0 v1 of
    Nothing -> 
      let
        newNode    = getFreshNumber b
        thNode     = ((), v0)
        elNode     = ((), v1)
        targetArcs = [thNode, elNode]
        sourceArcs = []
        ctx        = (sourceArcs, newNode, l, targetArcs)
        newbdd = ctx & b
      in
        put newbdd >> return newNode
    Just n -> return n;
}

evalExpression = undefined

build :: [String] -> Exp -> BDDState Node
build _ e = evalExpression e -- should return the nodes associated with constants
build (x:xs) e =
  let
    (e0, e1) = cofactors x e
  in
    do {
      v0 <- build xs e0;
      v1 <- build xs e1;
      mk x v0 v1
    }

buildBDD :: [String] -> Exp -> BDD
buildBDD vars e =
  execState (build vars e) (mkGraph [] [])

cofactors = undefined

duplicateExists :: BDD -> String -> Node -> Node -> Maybe Node
duplicateExists b l t e =
  let
    getIt :: Context NodeLabel EdgeLabel -> Maybe Node -> Maybe Node
    getIt _ a@(Just _) = a
    getIt c Nothing =
          let
            ctxHasLabel = (lab' c == l)
            ctxHasTNode = head (suc' c) == t
            ctxHasENode = (suc' c !! 1) == e
          in
            if ctxHasLabel && ctxHasTNode && ctxHasENode
            then
              Just (node' c)
            else
              Nothing
  in
    ufold getIt Nothing b





pdot d = unpack . DGP.renderDot . DGP.toDot . DG.graphToDot DG.nonClusteredParams $ d
pdotFinal n x =  plotDot n  $ pdot x
