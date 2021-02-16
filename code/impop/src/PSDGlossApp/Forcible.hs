module PSDGlossApp.Forcible where

import Control.Lens
import Data.Ext
import Data.Geometry.PlanarSubdivision.ForceDirected

import Convert

import PSDGlossApp.Common
import PSDGlossApp.Visible

import Graphics.Gloss.Interface.Pure.Game hiding (Point, Vector, Arc, Path, arc)

forcible :: (HasForceData v, RealFrac r) => ForceLogic s v e f r -> Behaviour s v e f r
--forcible l app = app { passTime = \d st -> passTime app d $ st { subdivision = forceStep l d $ subdivision st } }

-- forcible l = passTime %~ (\f d -> f d . (subdivision %~ forceStep l d))
forcible l s = s & passTime    %~~ forceTime l
                 & handleEvent %~~ forceEvent

forceTime :: (HasForceData v, RealFrac r) => ForceLogic s v e f r -> Float -> State s v e f r -> State s v e f r
forceTime l d s | s ^. frozen = s
                | otherwise   = s & (subdivision %~ forceStep l d)

forceEvent :: Event -> State s v e f r -> State s v e f r
forceEvent (EventKey (Char 'f') Down _ _) s = s & frozen %~ not
forceEvent _ s = s


forceDrawVert :: (HasForceData v, RealFrac r) => VertDrawer s v e f r
forceDrawVert _ _ v p = case view fluid v of
  Fluid         -> spot p white
  Fixed q       -> spot p red
  Constrained b -> spot p green
  
--spot :: Point 2 r -> Color -> Picture
spot p c = Pictures [ Color c     $ uncurry Translate (glossify p) $ circleSolid 3
                    , Color black $ uncurry Translate (glossify p) $ circle      3
                    ]