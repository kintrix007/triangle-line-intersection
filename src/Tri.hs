module Tri
  (Tri (Tri)
  , isOnTri) where

import           Vector3

data Tri = Tri
  !Vector3 -- ^ Vertex a
  !Vector3 -- ^ Vertex b
  !Vector3 -- ^ Vertex c
  deriving (Show, Eq)

isOnTri :: Vector3 -> Tri -> Bool
isOnTri p (Tri a b c) =
  checkSideAC p (Tri a b c)
  && checkSideAC p (Tri b c a)
  && checkSideAC p (Tri c a b)

checkSideAC :: Vector3 -> Tri -> Bool
checkSideAC p (Tri a b c) =
  vip `dot` vib >= 0
  where
    vab = b - a
    vac = c - a
    vap = p - a
    vai = vab `projectOn` vac
    vip = vap - vai
    vib = vab - vai

