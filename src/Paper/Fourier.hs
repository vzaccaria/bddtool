{-# LANGUAGE DataKinds #-}

module Paper.Fourier where

import Control.Monad
import Language.Operators
import Backend.UnitInterval.UnitInterval
import Data.List (intercalate)
import Data.Maybe
import qualified Data.Set as Set
import Utils.Plot

powerset :: [a] -> [[a]]
powerset = filterM (const [True,False])

chi :: Symantics r
    => [Int] -> [r (V 1)] -> r (V 1)
chi s x = foldl (.+) bot $ map (x !!) s

b :: (Symantics r)
  => Int -> [[r (V 1)]]
b 0 = [[bot],[top]]
b n = [x ++ y|x <- b 0,y <- b (n - 1)]

cf
  :: ([B (V 1)] -> Double) -> [Int] -> [B (V 1)] -> Double
cf f s x = f x * unB (chi s x)

fourier
  :: ([B (V 1)] -> Double) -> Int -> [([Int],Double)]
fourier f n =
  let bs = b (n - 1)
      pset = powerset [0 .. (n - 1)]
  in do s <- pset
        return (s,sum (map (cf f s) bs) / (2.0 ** fromIntegral n))

hw :: B (V n) -> Double
hw x =
  case unB x of
    -1.0 -> 1.0 :: Double
    1.0 -> 0.0
    _ -> error "!"

asis :: B (V n) -> Double
asis x = unB x :: Double

ff [x,y,z] = hw (x .+ y .+ z) ^ (3 :: Int)
-- ff [x,y,z] = hw (x .& y .& z) ^ (2 :: Int)
ff _ = error "!"

q :: [([Int],Double)]
q = fourier ff 3

cn :: [Int] -> String
cn [] = "E"
cn n =
  "v" ++
  intercalate ""
              (map show n)

percentBlack :: Double -> String
percentBlack v1 =
  let
    v1' = if v1 < 0 then -1 * v1 else v1
    v1'' = if v1' > 1 then 1 else v1'
  in "fill=black!" ++ show ( floor (v1'' * 100) :: Integer)

box :: [Int] -> Double -> String
box x v1 =
  cn x ++ "[" ++ percentBlack v1 ++ ", as=" ++ show v1 ++", font=\\tiny, rectangle, draw=black!20, minimum size=0.7cm]"

tikzg' l n =
  let l0 = filter (\x -> n == length (fst x)) l
      l1 = filter (\x -> (n - 1) == length (fst x)) l
      pairs =
        do (x, v1) <- l0
           (y, v2) <- l1
           if Set.fromList y `Set.isSubsetOf` Set.fromList x
              then return $ Just $ (box x v1) ++ " -> " ++ (box y v2)
              else return Nothing
  in pairs ++
     (if n > 1
         then tikzg' l (n - 1)
         else [])

tikzg :: [([Int],Double)] -> Int -> String
tikzg l n = intercalate ", " $ catMaybes $ tikzg' l n

graph x =
  "\\begin{tikzpicture}\\graph [layered layout] {" ++
  x ++ "};\\end{tikzpicture}"

writeExample = plotTikz "example.pdf" $ graph $ tikzg q 3
