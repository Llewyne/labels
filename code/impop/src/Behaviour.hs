{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Behaviour where

-- include specific behaviours in separate modules

import Graphics.Gloss hiding (Point, Vector, Arc, arc, display)
import Graphics.Gloss.Data.ViewPort 

import Graphics.Gloss.Interface.Pure.Game hiding (Point, Vector, Arc, arc)

import Control.Lens hiding (holesOf)

import Data.Foldable (toList)

import Data.Functor.Identity
import Data.Default
import Data.Ext
import Data.Geometry hiding (head, endPoints)
import Data.Geometry.PlanarSubdivision
import qualified Data.CircularSeq as C

import Data.Geometry.PlanarSubdivision.Dynamic
import Data.Geometry.PlanarSubdivision.More
import Data.Geometry.PlanarSubdivision.ForceDirected
import Data.PlanarGraph.Dart

import Algorithms.Geometry.Misc


import Convert
-- import Draw




standardDisplay :: Display
standardDisplay = InWindow "Window" (800, 800) (100, 100)

standardBackground :: Color
standardBackground = white

standardRate :: Int
standardRate = 20

type State s v e f r x = PlanarSubdivision s v e f r :+ x

data App s v e f r x = App
  { _drawState   :: State s v e f r x -> Picture
  , _handleEvent :: Event -> State s v e f r x -> State s v e f r x
  , _passTime    :: Float -> State s v e f r x -> State s v e f r x
  }
$(makeLenses ''App)

makeApp :: App s v e f r x
makeApp = App (const Blank) (const id) (const id)
  
run :: App s v e f r x -> State s v e f r x -> IO ()
run app st = play
  standardDisplay
  standardBackground
  standardRate
  st
  (app ^. drawState)
  (app ^. handleEvent)
  (app ^. passTime)


type Behaviour s v e f r x = App s v e f r x -> App s v e f r x





visible :: Behaviour s v e f r x -- might want to add colour information?
visible app = app & drawState %~ undefined

type DrawVert s v r   = RealFrac r => VertexId' s -> v -> Point 2 r         -> Picture
type DrawEdge s v e r = RealFrac r => Arc s       -> e -> LineSegment 2 v r -> Picture
type DrawFace s v f r = RealFrac r => FaceId' s   -> f -> SomePolygon v r   -> Picture

drawVertsWith :: RealFrac r => DrawVert s v r   -> Behaviour s v e f r x 
drawVertsWith f app = app & drawState %~ \op st -> Pictures $ op st : vps st
  where vps st = map (\(i, VertexData p v) -> f i v p) $ toList $ vertices $ st ^. core

drawEdgesWith :: RealFrac r => DrawEdge s v e r -> Behaviour s v e f r x 
drawEdgesWith f app = app & drawState %~ \op st -> Pictures $ op st : eps st
  where eps st = map (\(i, s :+ e) -> f (i ^. arc) e s) $ toList $ edgeSegments $ st ^. core

drawFacesWith :: RealFrac r => DrawFace s v f r -> Behaviour s v e f r x 
drawFacesWith f app = app & drawState %~ \op st -> Pictures $ op st : fps st
  where fps st = map (\(i, p :+ e) -> f i e p) $ toList $ rawFacePolygons $ st ^. core


basicDrawVert :: DrawVert s v r
basicDrawVert _ _ p = Color black $ uncurry Translate (glossify p) $ circleSolid 3

basicDrawEdge :: DrawEdge s v e r
basicDrawEdge _ _ s = Color black $ glossify s

basicDrawFace :: DrawFace s v f r
basicDrawFace _ _ _ = Blank

-- add basic drawing functionality
-- visible :: (RealFrac r, HasPSD s v e f r a) => GlossApp a -> GlossApp a
basicVisible :: RealFrac r => Behaviour s v e f r x 
basicVisible = drawVertsWith basicDrawVert . drawEdgesWith basicDrawEdge . drawFacesWith basicDrawFace
-- ^ split into basic version and version where you specify how to draw?











data PSE = V Int | D Int | F Int | N deriving (Eq, Ord, Show)


locate :: RealFrac r => PlanarSubdivision s v e f r -> Point 2 r -> PSE 
locate psd p = head $ (++ [N]) $ filter (/= N) 
  $  map (vertexScore p) (toList $ vertices psd)
  ++ map (edgeScore p) (toList $ edgeSegments psd)
  ++ map (faceScore p) (toList $ rawFacePolygons psd)

-- should make efficient implementation for point location - note this is a special kind that dilates vertices and edges

vertexScore :: (Num r, Ord r) => Point 2 r -> (VertexId' s, VertexData r v) -> PSE 
vertexScore p (i, VertexData q _) | d < 15^2  = V $ fromEnum i
                                  | otherwise = N
  where d = squaredEuclideanDist p q

edgeScore :: RealFrac r => Point 2 r -> (Dart s, LineSegment 2 v r :+ e) -> PSE 
edgeScore p (i, s :+ _) | insidePolygon p $ dilate s 5 = D $ fromEnum i
                        | otherwise                    = N

faceScore :: RealFrac r => Point 2 r -> (FaceId' s, SomePolygon v r :+ f) -> PSE 
faceScore p (i, q :+ _) | either (insidePolygon p) (insidePolygon p) q = F $ fromEnum i
                        | otherwise         = N

-- this is for hover and selection behaviour (I guess can be kept separate but still using the same data type and in the same module)
data SelectionData = SelectionData { _ho :: PSE, _se :: [PSE] }
$(makeLenses ''SelectionData)

class HasSelectionData x where
  hover :: Lens' x PSE
  selection :: Lens' x [PSE]

instance HasSelectionData SelectionData where
  hover = ho
  selection = se

instance HasSelectionData x => HasSelectionData (y :+ x) where
  hover = extra . hover
  selection = extra . selection

{-
instance HasExtra x (SelectionData s) => HasSelectionData s x where
  hover = extra . ho
  selection = extra . se
-}

-- other behaviours include:
  -- force-directed
  -- editing (which relies on selection)
  -- debug (writing to stdout) (relies on hover)
  -- annotated drawings




hoverDrawVert :: DrawVert s v r
hoverDrawVert i v p = Color yellow $ uncurry Translate (glossify p) $ circleSolid 15

hoverDrawEdge :: DrawEdge s v e r
hoverDrawEdge i e s = Color yellow $ glossify $ dilate s 5

hoverDrawFace :: DrawFace s v f r
hoverDrawFace i f p = Color yellow $ glossify $ erode p 5

--drawHover :: RealFrac r => PlanarSubdivision s v e f r -> PSE s -> Picture
--drawHover psd N     = Blank
--drawHover psd (V i) = drawAt (psd ^. locationOf i) hovV
--drawHover psd (D i) = hovE (psd ^. locationOf (psd & headOf i)) (psd ^. locationOf (psd & tailOf i))


hoverable :: (HasSelectionData x, RealFrac r) => Behaviour s v e f r x
--hoverable :: RealFrac r => App s v e f r -> App s v e f r
-- ^ should be more general
hoverable app = app
  { _drawState   = \st -> Pictures [drawHover (st ^. core) (st ^. hover), _drawState app st]
  , _handleEvent = \e -> handleHover e . _handleEvent app e
--  , _passTime    = passTime app -- check for update?
  }
  where
--    drawHover :: RealFrac r => PlanarSubdivision s v e f r -> PSE s -> Picture
    drawHover psd N     = Blank
    drawHover psd (V i) = hoverDrawVert (v i) (psd ^. dataOf (v i)) (psd ^. locationOf (v i))
--    drawHover psd (D i) = hoverDrawEdge (_arc i) (psd ^. dataOf i) $ _core $ edgeSegment i psd
--    drawHover psd (F i) = hoverDrawFace i (psd ^. dataOf i) $ _core $ rawFacePolygon i psd

--    handleHover :: Event -> State s v e f r -> State s v e f r
    handleHover (EventMotion (x, y)) s = 
      let h = s ^. hover
          n = locate (s ^. core) $ traverse %~ realToFrac $ Point2 x y
      in  s & hover .~ n
    handleHover _ s = s

    v :: Int -> VertexId' s
    v = toEnum


-- should we make explicit depth order? we like hover below selection below normal drawing, but selection behaviour is added last!
-- or, visualize selection differently?

{-

selecDrawVert :: DrawVert s v r
selecDrawVert i v p = Color orange $ uncurry Translate (glossify p) $ circleSolid 17

selecDrawEdge :: DrawEdge s v e r
selecDrawEdge i e s = Color orange $ glossify $ dilate s 7

selecDrawFace :: DrawFace s v f r
selecDrawFace i f p = Color orange $ glossify $ erode p 7


selectable :: (HasSelectionData x, RealFrac r) => Behaviour s v e f r x 
selectable app = app
  { _drawState   = \s -> Pictures $ (map (drawSelection $ s ^. core) $ s ^. selection) ++ [_drawState app s]
  , _handleEvent = \e -> handleSelection e . _handleEvent app e
--  , passTime    = passTime app
  }
  where
    drawSelection :: RealFrac r => PlanarSubdivision s v e f r -> PSE s -> Picture
    drawSelection psd (V i) = selecDrawVert i (psd ^. dataOf i) (psd ^. locationOf i)
    drawSelection psd (D i) = selecDrawEdge (_arc i) (psd ^. dataOf i) $ _core $ edgeSegment i psd
    drawSelection psd (F i) = selecDrawFace i (psd ^. dataOf i) $ _core $ rawFacePolygon i psd
    drawSelection psd N     = Blank

    handleSelection (EventKey (MouseButton LeftButton) Down _ _) s
      | s ^. hover == N                  = s
      | s ^. hover `elem` s ^. selection = s & selection %~ filter (/= s ^. hover)
      | otherwise                        = s & selection %~ (s ^. hover :)
    handleSelection _ s = s

-}