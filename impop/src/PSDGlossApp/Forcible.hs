module PSDGlossApp.Forcible where

import Control.Lens
import Data.Extra
import Data.Geometry.PlanarSubdivision.ForceDirected

import PSDGlossApp.Common


forcible :: (HasForceData v, RealFrac r) => ForceLogic s -> Behaviour s v e f r
--forcible l app = app { passTime = \d st -> passTime app d $ st { subdivision = forceStep l d $ subdivision st } }

--forcible l = passTime %~ (\f d -> f d . (subdivision %~ forceStep l d))
forcible l = passTime %~~ (\d -> (subdivision %~ forceStep l d))

