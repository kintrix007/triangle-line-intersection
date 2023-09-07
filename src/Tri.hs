module Tri
  (Tri (Tri)
  , solve
  , isOnTri) where

import           Vector3
import Debug.Trace (trace)

data Tri = Tri
  { a :: !Vector3
  , b :: !Vector3
  , c :: !Vector3 }
  deriving (Show, Eq)

isOnTri :: Vector3 -> Tri -> Bool
isOnTri vap (Tri a b c) =
  case do {
      (x1, x2) <- solve vab vac vap;
      if x1 >= 0 && x2 >= 0
        then solve vba vbc vbp
        else Nothing;
    } of 
    Nothing       -> False
    Just (x1, x2) -> x1 >= 0 && x2 >= 0
  where
    vab = b - a
    vac = c - a
    vba = a - b
    vbc = c - b
    vbp = vap - vab

solve :: Vector3 -> Vector3 -> Vector3 -> Maybe (Double, Double)
solve (Vector3 a1 b1 c1) (Vector3 a2 b2 c2) (Vector3 x y z)
  | thing == 0 && False = Nothing
  | otherwise = Just (x1, x2)
  where
    thing = z - c1/a1*x - ((c2 - c1/a1*a2) / (b2 - b1/a1*a2))*(y - b1/a1*x)
    x1 = (x - a2/(b2 - b1/a1*a2)*y - b1/a1*x) / a1
    x2 = (y - b1/a1*x) / (b1 - b1/a1*a2)

