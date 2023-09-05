module Tri
  (Tri (Tri)
  ) where

import           Vector3

data Tri = Tri
  { a :: !Vector3
  , b :: !Vector3
  , c :: !Vector3 }
  deriving (Show, Eq)
