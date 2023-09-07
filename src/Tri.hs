module Tri
  (Tri (..)
  , isOnTri
  , Plane (..)
  , Line (..)
  , planeOf
  , triLineIntersects) where

import           Vector3

data Tri = Tri
  !Vector3 -- ^ Vertex a
  !Vector3 -- ^ Vertex b
  !Vector3 -- ^ Vertex c
  deriving (Show, Eq)

data Plane = Plane
  !Vector3 -- ^ Normal of the plane
  !Vector3 -- ^ A point on the plane
  deriving (Show, Eq)

data Line = Line
  !Vector3 -- ^ Direction of the line
  !Vector3 -- ^ A point on the line
  deriving (Show, Eq)

data LineIntersectionResult
  = NoIntersection
  | LineContained
  | IntersectionPoint !Vector3
  deriving (Show)

triLineIntersects :: Tri -> Line -> Bool
triLineIntersects tri line =
  case intersect of
    NoIntersection      -> False
    LineContained       -> True
    IntersectionPoint p -> isOnTri p tri
  where
    intersect = planeLineIntersection (planeOf tri) line

planeOf :: Tri -> Plane
planeOf (Tri a b c) = Plane normal a
  where
    vab = b - a
    vac = c - a
    normal = vab `cross` vac

planeLineIntersection :: Plane -> Line -> LineIntersectionResult
planeLineIntersection (Plane pn pp) (Line lv lp) =
  if isParallel
    then if isLineContained
      then LineContained
      else NoIntersection
    else IntersectionPoint intersectionPoint
  where
    isParallel = lv `dot` pn == 0
    isLineContained = (pp - lp) `dot` pn == 0
    intersectionPoint = lp + lv*toVec d
    d = ((pp - lp) `dot` pn) / (lv `dot` pn)

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

