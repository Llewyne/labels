{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Geometry.PlanarSubdivision.ForceDirected
  ( ForceData (ForceData)
  , HasForceData
  , ForceVector
  , ForceLogic
  , Fluidity (Fluid, Fixed, Constrained)
  , forceful
  , fluid
  , force
  , (+++)
  , simpleForces
  , curvedForces
  , resetForces
  , computeForces
  , applyForces
  , forceStep
  , forceStep'
  ) where

import Control.Lens
import Data.Vector (toList)
import Data.Default
import Data.Extra
import Data.Geometry
import Data.Geometry.Bezier
import Data.Geometry.PlanarSubdivision
import Algorithms.Geometry.Misc


-- TODO:
-- prevent topological changes
-- constrain vertices to lie on segments or curves


-- | Setting 'fluid' to 'Fixed' prevents a vertex from being moved.
data Fluidity = Fluid | Fixed (Point 2 Float) | Constrained (Bezier 2 Float) Float
  deriving (Show, Eq)

type ForceVector = Vector 2 Float

-- | Forces are explicitly modeled as vectors of 'Floats', 
--   since iterated manipulation of exact number types will
--   make your life miserable.
class HasForceData v where
  fluid :: Lens' v Fluidity
  force :: Lens' v ForceVector

data ForceData = ForceData { _fl :: Fluidity, _ve :: ForceVector }
  deriving (Show, Eq)
$(makeLenses ''ForceData)

{-
instance HasForceData ForceData where
  fluid = fl
  force = ve
-}

-- instance {-# OVERLAPS #-} HasExtra v ForceData => HasForceData v where
instance {-# OVERLAPS #-} HasForceData (v :+ ForceData) where
  fluid = extra . fl
  force = extra . ve

instance Default ForceData where
  def = ForceData Fluid (origin .-. origin)


-- | Make a 'PlanarSubdivision' have 'ForceData'.
forceful :: PlanarSubdivision s v e f r -> PlanarSubdivision s (v :+ ForceData) e f r
forceful = vertexData . traverse %~ (:+ def)

-- | A description of how to calculate force vectors in the neighbourhood of a given vertex.
type ForceLogic s = forall v e f r. Real r => PlanarSubdivision s v e f r -> VertexId' s -> [(VertexId' s, ForceVector)]
(l1 +++ l2) psd i = l1 psd i ++ l2 psd i

-- | Set all force vectors to zero.
resetForces :: HasForceData v => PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
resetForces = vertexData . traverse . force .~ (origin .-. origin)

-- | Move all vertex by a 'fraction' of their force vectors.
applyForces :: (HasForceData v, RealFrac r) => Float -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
applyForces d psd = foldr ($) psd $ fmap (applyForce d) $ vertices' psd

applyForce :: (HasForceData v, RealFrac r) => Float -> VertexId' s -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
applyForce d i psd = let q = (traverse %~ realToFrac $ psd ^. locationOf i) .+^ (d *^ psd ^. dataOf i . force)
  in case psd ^. dataOf i . fluid of
    Fluid           -> psd & moveVertex i (traverse %~ realToFrac $ q)
    Fixed p         -> psd
    Constrained b t -> psd & moveVertex i (traverse %~ realToFrac $ snap b q)
                           & dataOf i . fluid .~ (Constrained b $ parameterOf b q)

moveVertex :: VertexId' s -> Point 2 r -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
moveVertex i p psd = psd & locationOf i .~ p
-- use appropriate function to actually move the vertex? i.e. updating also edge stubs
  -- update all v's and e's (and f's) based on new location

{-
updateV :: PlanarSubdivision s v e f r -> VertexId' s -> v -> v  
updateE :: PlanarSubdivision s v e f r -> Dart s -> e -> e  
updateF :: PlanarSubdivision s v e f r -> FaceId' s -> f -> f  
-}

-- | Calculate force vectors according to the given 'logic'.
computeForces :: (HasForceData v, Real r) => ForceLogic s -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
computeForces logic psd = foldr (\(i, v) -> dataOf i . force %~ (^+^ v)) psd $ concatMap (logic psd) $ toList $ vertices' psd

-- | Each vertex is attracted towards the average of its neighbours.
simpleForces :: ForceLogic s 
simpleForces psd i = let p = traverse %~ realToFrac $ psd ^. locationOf i
                         ps = fmap (\j -> traverse %~ realToFrac $ psd ^. locationOf j) $ neighboursOf i psd
                     in [(i, average ps .-. p)]

-- | Each pair of opposite edges of a vertex try to become as straight as possible.
curvedForces :: ForceLogic s 
curvedForces psd i = concatMap (straighten psd . interpose i) $ opposites $ toList $ neighboursOf i psd

straighten :: Real r => PlanarSubdivision s v e f r -> (VertexId' s, VertexId' s, VertexId' s) -> [(VertexId' s, ForceVector)]
straighten psd (i, j, k) = let v = traverse %~ realToFrac $ (psd ^. locationOf i .-. psd ^. locationOf j) ^+^ (psd ^. locationOf k .-. psd ^. locationOf j)
                           in [(i, (-0.5) *^ v), (j, v), (k, (-0.5) *^ v)]

opposites :: [a] -> [(a, a)]
opposites ls | even $ length ls = zip ls $ drop (length ls `div` 2) ls
             | otherwise = []

interpose :: a -> (a, a) -> (a, a, a)
interpose b (a, c) = (a, b, c)

-- | Perform a complete step using 'logic' to calculate force vectors, and moving all vertices by a 'fraction' in $[0,1]$ along their calculated force vectors.
forceStep :: (HasForceData v, RealFrac r) => ForceLogic s -> Float -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
forceStep logic fraction = applyForces fraction . computeForces logic . resetForces

-- | Perform a step on a 'PlanarSubdivision' without 'ForceData'. All vertices will move.
forceStep' :: RealFrac r => ForceLogic s -> Float -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
forceStep' logic fraction = (vertexData . traverse %~ _core) . forceStep logic fraction . forceful
