{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes       #-}

module Paper.Fourier where

import           Data.String.Interpolate
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


cn :: [Int] -> String
cn [] = "E"
cn n =
  "v" ++
  intercalate ""
              (map show n)

percentBlack :: Double -> String
percentBlack v1 =
  let v1' =
        if v1 < 0
           then -1 * v1
           else v1
      v1'' =
        if v1' > 1
           then 1
           else v1'
  in "fill=black!" ++ show (floor (v1'' * 100) :: Integer)

box :: [Int] -> Double -> Int -> String
box x v1 nsen =
  let
    color = if (all (\q -> q < nsen) x) && (x /= []) then "red" else "black!10"
  in
    [i| #{cn x} [#{percentBlack v1}, as=#{show v1}, draw=#{color}, font=\\tiny, circle, thin, minimum size=0.5cm] |]

tikzg' l n nsen =
  let l0 = filter (\x -> n == length (fst x)) l
      l1 = filter (\x -> (n - 1) == length (fst x)) l
      pairs =
        do (x,v1) <- l0
           (y,v2) <- l1
           if Set.fromList y `Set.isSubsetOf` Set.fromList x
              then return $ Just $ (box x v1 nsen) ++ " -> " ++ (box y v2 nsen)
              else return Nothing
  in pairs ++
     (if n > 1
         then tikzg' l (n - 1) nsen
         else [])

tikzg :: [([Int],Double)] -> Int -> Int -> String
tikzg l n nsen = intercalate ", " $ catMaybes $ tikzg' l n nsen

graph x =
  "\\begin{tikzpicture}\\graph [layered layout] {" ++
  x ++ "};\\end{tikzpicture}"


-- Here comes the example


tr [s1, t1, m2, m3, m4, m5, m6, n2, n3, n4, n5, n6] =
  let 
    x 1 = s1 .+ m2 .+ m3 .+ m4 .+ m5 .+ m6
    x 2 = m2
    x 3 = m3
    x 4 = m4
    x 5 = m5

    y 1 = t1 .+ n2 .+ n3 .+ n4 .+ n5 .+ n6
    y 2 = n2
    y 3 = n3
    y 4 = n4
    y 5 = n5
    y 6 = n6
    a 2 = (x 3 .& y 3) .+ (x 3 .& y 4) .+ (x 4 .& y 3) .+ (x 3 .& y 5) .+ (x 5 .& y 3)
  in
    hw $ a 2

-- a 3 = (x 4 .& y 4) .+ (x 2 .& y 4) .+ (x 4 .& y 2) .+ (x 2 .& y 6) .+ (x 6 .& y 2)
-- a 4 = (x 5 .& y 5) .+ (x 1 .& y 4) .+ (x 4 .& y 1) .+ (x 1 .& y 5) .+ (x 5 .& y 1)

ff [x,y,z] = hw (x .+ y .+ z) ^ (3 :: Int)
ff _ = error "!"

drawFourier f nvar nsen =
  let
    t = fourier f nvar
    d = tikzg t nvar nsen
  in
    d

writeExample = plotTikz "example.pdf" $ graph $ drawFourier tr 12 2 
