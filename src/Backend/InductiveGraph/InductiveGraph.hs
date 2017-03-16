{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Backend.InductiveGraph.InductiveGraph where

import           Control.Arrow
import           Control.Monad.State.Lazy hiding (lift)
import           Data.Bool                (Bool (..))
import           Data.Graph.Inductive
import           Data.List                (foldl)
import           Data.Maybe               ()
import           Language.Operators

xor :: Bool -> Bool -> Bool
xor a b = not a && b || a && not b

-- E is a type function that will collapse to our internal expression type
-- We are going to make E an instance of our symantics
data family E a

data instance E (V n) = E{unE :: Exp}

data Exp
  = ETRUE
  | EFALSE
  | EVAR String
  | EAND Exp
         Exp
  | EOR Exp
        Exp
  | EXOR Exp
         Exp
  | ENEG Exp
  | EMELD Exp
          Exp
  deriving (Show,Eq)

instance Symantics E where
  bot = E EFALSE
  top = E ETRUE
  var = E . EVAR
  neg = E . ENEG . unE
  x .& y = E (unE x `EAND` unE y)
  x .+ y = E (unE x `EXOR` unE y)
  x .| y = E (unE x `EOR` unE y)
  f .<> e = E (unE f `EMELD` unE e)

partialEval :: Maybe (String,Exp) -> Exp -> Exp
partialEval Nothing e = e
partialEval _ ETRUE = ETRUE
partialEval _ EFALSE = EFALSE
partialEval (Just (vr,vv)) a@(EVAR s) =
  if s == vr
     then vv
     else a
partialEval c (EAND e1 e2) =
  EAND (partialEval c e1)
       (partialEval c e2)
partialEval c (EOR e1 e2) =
  EOR (partialEval c e1)
      (partialEval c e2)
partialEval c (EXOR e1 e2) =
  EXOR (partialEval c e1)
       (partialEval c e2)
partialEval c (ENEG e1) = ENEG (partialEval c e1)
partialEval c (EMELD e1 e2) =
  EMELD (partialEval c e1)
        (partialEval c e2)

eval :: Exp -> [Bool]
eval (EVAR s) = error ("Sorry, " ++ show s ++ " cant be fully evaluated")
eval ETRUE = [True]
eval EFALSE = [False]
eval (EAND e1 e2) = [head (eval e1) && head (eval e2)]
eval (EOR e1 e2) = [head (eval e1) || head (eval e2)]
eval (EXOR e1 e2) = [head (eval e1) `xor` head (eval e2)]
eval (ENEG e1) = [not $ head $ eval e1]
eval (EMELD e1 e2) = eval e1 ++ eval e2

cofactors :: String -> Exp -> (Exp,Exp)
cofactors v e =
  let e0 =
        partialEval (Just (v,ETRUE))
                    e
      e1 =
        partialEval (Just (v,EFALSE))
                    e
  in (e0,e1)

data InternalFun
  = ITE String
        InternalFun
        InternalFun
  | TERM [Bool]
  deriving (Eq,Show)

data NodeLabel = Vr String | D [Bool] deriving (Eq)

instance Show NodeLabel where
  show (Vr s) = s
  show (D b) = sbv b

type EdgeLabel = Bool

type BDD = Gr NodeLabel EdgeLabel

type BDDState a = State BDD a

getFreshNumber :: Graph gr
               => gr a b -> Int
getFreshNumber = noNodes

sbv :: [Bool] -> String
sbv = concatMap (\x -> if x then "1" else "0")

mkConst :: [Bool] -> BDDState Node
mkConst bv =
  do b <- get
     case findConstNode b bv of
       Nothing ->
         let newNode = getFreshNumber b
             ctx = ([],newNode, D bv,[])
             newbdd = ctx & b
         in put newbdd >> return newNode
       Just n -> return n

mk :: NodeLabel -> Node -> Node -> BDDState Node
mk l v0 v1 =
  do b <- get
     if v0 == v1
        then return v0
        else case duplicateExists b l v0 v1 of
               Nothing ->
                 let newNode = getFreshNumber b
                     thNode = (True,v0)
                     elNode = (False,v1)
                     targetArcs = [thNode,elNode]
                     sourceArcs = []
                     ctx = (sourceArcs,newNode,l,targetArcs)
                     newbdd = (ctx & b)
                 in put newbdd >> return newNode
               Just n -> return n

build :: [String] -> Exp -> BDDState Node
build [] e = mkConst $ eval e -- should return the nodes associated with constants
build (x:xs) e =
  let (e0,e1) = cofactors x e
  in do v0 <- build xs e0
        v1 <- build xs e1
        mk (Vr x) v0 v1

buildBDD :: [String] -> Exp -> BDD
buildBDD vars e =
  execState (build vars e)
            (mkGraph [] [])

findConstNode :: BDD -> [Bool] -> Maybe Node
findConstNode b bv =
  let getIt
        :: Context NodeLabel EdgeLabel -> Maybe Node -> Maybe Node
      getIt _ a@(Just _) = a
      getIt c Nothing =
        if lab' c == D bv
           then Just (node' c)
           else Nothing
  in ufold getIt Nothing b

duplicateExists
  :: BDD -> NodeLabel -> Node -> Node -> Maybe Node
duplicateExists b l t e =
  let getIt a@(Just _) _ = a
      getIt Nothing n =
        let c = context b n
            ctxHasLabel = (lab' c == l)
            ctxHasTNode = head (suc' c) == t
            ctxHasENode = (suc' c !! 1) == e
        in if ctxHasLabel && ctxHasTNode && ctxHasENode
              then Just (node' c)
              else Nothing
  in foldl getIt Nothing (nodes b)

edgeTransform :: (DynGraph gr) => (a -> b -> c) -> gr a b -> gr a c
edgeTransform f = gmap (\(p,v,l,_)->(map1 (f l) p,v,l,[]))
  where
    map1 g = map (first g)  

findRootNode :: Graph gr => gr a b -> Node
findRootNode b = let
    nds = nodes b
    rts = filter (null . inn b) nds
  in
    head rts




