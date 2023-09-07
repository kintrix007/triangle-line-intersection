module Tri
  (Tri (Tri)
  , isOnTri) where

import           Vector3

data Tri = Tri
  { a :: !Vector3
  , b :: !Vector3
  , c :: !Vector3 }
  deriving (Show, Eq)

-- isOnTri :: Vector3 -> Tri -> Bool
isOnTri p (Tri a b c) =
  -- Only checks one side
  vip `dot` vib
  where
    vab = b - a
    vac = c - a
    vap = p - a
    vai = vab `projectOn` vac
    vip = vap - vai
    vib = vab - vai

