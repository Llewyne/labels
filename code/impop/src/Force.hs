{-# LANGUAGE TemplateHaskell #-}

-- {-# LANGUAGE IncoherentInstances #-}

module Force where

import Graphics.Gloss hiding (Point, Vector, Arc, arc, display)
import Graphics.Gloss.Data.ViewPort 

import Graphics.Gloss.Interface.Pure.Game hiding (Point, Vector, Arc, arc)

import Control.Lens hiding (holesOf)

import Data.Vector ((!))
import Data.Foldable (toList)

import Data.Functor.Identity
import Data.Default
import Data.Extra
import Data.Geometry hiding (head, endPoints)
import Data.Geometry.Bezier
import Data.Geometry.PlanarSubdivision
import qualified Data.CircularSeq as C

--import Data.PlanarGraph (PlanarGraph)
import Data.PlaneGraph (PlaneGraph, fromAdjRep, toAdjRep)
import Data.PlaneGraph.AdjRep hiding (id, vData, faces)
import qualified Data.PlaneGraph.AdjRep as A (id, vData, faces)
import Data.PlanarGraph.Dart
import Debug.Trace

import Data.Geometry.PlanarSubdivision.Dynamic
import Data.Geometry.PlanarSubdivision.More
import Data.Geometry.PlanarSubdivision.ForceDirected

import Algorithms.Geometry.Misc


import Convert
import Draw


standardDisplay :: Display
standardDisplay = InWindow "Window" (800, 800) (100, 100)

standardBackground :: Color
standardBackground = white

standardRate :: Int
standardRate = 20

data Whatever = Whatever

--class ShowId a where showid :: a -> String


{-}
instance ShowId (FaceId w s) where showid f = 'f' : show (fromEnum f)
instance ShowId a => ShowId [a] where showid xs = show $ map showid xs
instance (Show a, ShowId b) => ShowId (a, b) where showid (a, b) = show (a, showid b)
-}


data State s v e f r = State
  { subdivision  :: PlanarSubdivision s v e f r 
  , hover        :: PSE s
  , hoverChanged :: Bool
  , selection    :: [PSE s]
  }

-- maybe a state should really be a PlanarSubdivision s v e f r :+ extra
-- or maybe not. because: convincing behaviours that a state has PSE's of the right s
-- cannot seem to be done through type classes

data App s v e f r = App
  { drawState   :: State s v e f r -> Picture
  , handleEvent :: Event -> State s v e f r -> State s v e f r
  , passTime    :: Float -> State s v e f r -> State s v e f r
  }

app :: App s v e f r
app = App (const Blank) (const id) (const id)
  
run :: (App s v e f r -> App s v e f r) -> PlanarSubdivision s v e f r -> IO ()
run f psd = play
  standardDisplay
  standardBackground
  standardRate
  (State psd N True [])
  (drawState $ f app)
  (handleEvent $ f app)
  (passTime $ f app)




type Behaviour s v e f r = App s v e f r -> App s v e f r
-- maybe reconsider changing the extra app data?


type DrawVert s v r   = RealFrac r => VertexId' s -> v -> Point 2 r         -> Picture
type DrawEdge s v e r = RealFrac r => Arc s       -> e -> LineSegment 2 v r -> Picture
type DrawFace s v f r = RealFrac r => FaceId' s   -> f -> SomePolygon v r   -> Picture

drawVertsWith :: RealFrac r => DrawVert s v r   -> App s v e f r -> App s v e f r
drawEdgesWith :: RealFrac r => DrawEdge s v e r -> App s v e f r -> App s v e f r
drawFacesWith :: RealFrac r => DrawFace s v f r -> App s v e f r -> App s v e f r

drawVertsWith f app = app { drawState = \st -> Pictures $ drawState app st : vps st }
  where vps st = map (\(i, VertexData p v) -> f i v p) $ toList $ vertices $ subdivision st

drawEdgesWith f app = app { drawState = \st -> Pictures $ drawState app st : eps st }
  where eps st = map (\(i, s :+ e) -> f (i ^. arc) e s) $ toList $ edgeSegments $ subdivision st

drawFacesWith f app = app { drawState = \st -> Pictures $ drawState app st : fps st }
  where fps st = map (\(i, p :+ e) -> f i e p) $ toList $ rawFacePolygons $ subdivision st


basicDrawVert :: DrawVert s v r
basicDrawVert _ _ p = Color black $ uncurry Translate (glossify p) $ circleSolid 3

basicDrawEdge :: DrawEdge s v e r
basicDrawEdge _ _ s = Color black $ glossify s

basicDrawFace :: DrawFace s v f r
basicDrawFace _ _ _ = Blank

-- add basic drawing functionality
-- visible :: (RealFrac r, HasPSD s v e f r a) => GlossApp a -> GlossApp a
basicVisible :: RealFrac r => App s v e f r -> App s v e f r
basicVisible = drawVertsWith basicDrawVert . drawEdgesWith basicDrawEdge . drawFacesWith basicDrawFace
-- ^ split into basic version and version where you specify how to draw?




drawAnnotation :: (Central c, RealFrac r, Enum i) => Color -> i -> e -> c r -> Picture
drawAnnotation c i _ p = uncurry Translate (glossify $ center p) $ Pictures
                       [ Color c $ circleSolid $ 100 * fontsize
                       , Color white $ draw $ show $ fromEnum i
                       , Color black $ circle $ 100 * fontsize
                       ] 

annotDrawVert :: DrawVert s v r
annotDrawVert = drawAnnotation blue

annotDrawEdge :: DrawEdge s v e r
annotDrawEdge = drawAnnotation red

annotDrawFace :: DrawFace s v f r
annotDrawFace i e f = drawAnnotation green i e $ either id asSimplePolygon f

annotVisible :: RealFrac r => App s v e f r -> App s v e f r
annotVisible = drawVertsWith annotDrawVert . drawEdgesWith annotDrawEdge . drawFacesWith annotDrawFace

-- hoverable = 


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


hoverable :: RealFrac r => App s v e f r -> App s v e f r
--hoverable :: RealFrac r => App s v e f r -> App s v e f r
-- ^ should be more general
hoverable app = app
  { drawState   = \s -> Pictures [drawHover (subdivision s) (hover s), drawState app s]
  , handleEvent = \e -> handleHover e . handleEvent app e
  , passTime    = passTime app -- check for update?
  }
  where
--    drawHover :: RealFrac r => PlanarSubdivision s v e f r -> PSE s -> Picture
    drawHover psd N     = Blank
    drawHover psd (V i) = hoverDrawVert i (psd ^. dataOf i) (psd ^. locationOf i)
    drawHover psd (D i) = hoverDrawEdge (_arc i) (psd ^. dataOf i) $ _core $ edgeSegment i psd
    drawHover psd (F i) = hoverDrawFace i (psd ^. dataOf i) $ _core $ rawFacePolygon i psd


--    handleHover :: Event -> State s v e f r -> State s v e f r
    handleHover (EventMotion (x, y)) s = 
      let h = hover s
          n = locate (subdivision s) $ traverse %~ realToFrac $ Point2 x y
      in  s { hover = n, hoverChanged = h /= n }
    handleHover _ s = s

-- should we make explicit depth order? we like hover below selection below normal drawing, but selection behaviour is added last!
-- or, visualize selection differently?

hoverableCurves :: RealFrac r => App s v e f r -> App s v e f r
--hoverable :: RealFrac r => App s v e f r -> App s v e f r
-- ^ should be more general
hoverableCurves app = app
  { drawState   = \s -> Color red $ Pictures [drawState app s, drawCurve (subdivision s) (hover s)] }
  where
--    curveVerts psd i = flip tailOf psd (head $ traceCurve psd i) : (map (flip headOf psd) $ traceCurve psd i)
    drawCurve psd (D i) = Pictures $ map (drawCurveSegment psd) $ traceCurve psd i
--    drawCurve psd (D i) = Color red $ glossify $ Bezier $ map ((psd ^.) . locationOf) $ curveVerts psd i
    drawCurve psd _     = Blank
    drawCurveSegment psd i = curveDrawEdge (_arc i) (psd ^. dataOf i) $ _core $ edgeSegment i psd


curveDrawEdge :: DrawEdge s v e r
curveDrawEdge i e s = Color red $ glossify $ dilate s 2



selecDrawVert :: DrawVert s v r
selecDrawVert i v p = Color orange $ uncurry Translate (glossify p) $ circleSolid 17

selecDrawEdge :: DrawEdge s v e r
selecDrawEdge i e s = Color orange $ glossify $ dilate s 7

selecDrawFace :: DrawFace s v f r
selecDrawFace i f p = Color orange $ glossify $ erode p 7


selectable :: RealFrac r => App s v e f r -> App s v e f r
selectable app = app
  { drawState   = \s -> Pictures $ (map (drawSelection $ subdivision s) $ selection s) ++ [drawState app s]
  , handleEvent = \e -> handleSelection e . handleEvent app e
  , passTime    = passTime app
  }
  where
    drawSelection :: RealFrac r => PlanarSubdivision s v e f r -> PSE s -> Picture
    drawSelection psd (V i) = selecDrawVert i (psd ^. dataOf i) (psd ^. locationOf i)
    drawSelection psd (D i) = selecDrawEdge (_arc i) (psd ^. dataOf i) $ _core $ edgeSegment i psd
    drawSelection psd (F i) = selecDrawFace i (psd ^. dataOf i) $ _core $ rawFacePolygon i psd
    drawSelection psd N     = Blank

    handleSelection (EventKey (MouseButton LeftButton) Down _ _) s
      | hover s == N               = s
      | hover s `elem` selection s = s {selection = filter (/= hover s) $ selection s}
      | otherwise                  = s {selection = hover s : selection s}
    handleSelection _ s = s






forcedirected :: (HasExtra v ForceData, RealFrac r) => ForceLogic s -> App s v e f r -> App s v e f r
forcedirected l app = app { passTime = \d st -> passTime app d $ st { subdivision = forceStep l d $ subdivision st } }





-- should make this monadic

-- add functionality for hovering mouse over elements of subdivision





-- add functionality for selecting subset of elements


--bla :: (RealFrac r, HasPSD s v e f r a) => GlossApp a -> (a, PSE s) -> Picture
--bla app (a, h) = Pictures [drawState app a]
--bla app (a, h) = Pictures [drawHover (getPSD a) h, drawState app a]


-- drawHover :: RealFrac r => PlanarSubdivision s v e f r -> PSE s -> Picture

-- drawSelection :: RealFrac r => PlanarSubdivision s v e f r -> PSE s -> Picture


-- could also stay closer to drawing functions:
-- handleHover :: RealFrac r => PlanarSubdivision s v e f r -> PSE s -> PSE s



editable :: (Default v, Default e, Show v, Show e, Show f, Show r, RealFrac r) => App s v e f r -> App s v e f r
editable app = app
  { handleEvent = \e -> handleEdit e . handleEvent app e
  }
  where
    handleEdit (EventKey (SpecialKey KeySpace) Down _ _) s = traceShow (selection s) $
      let l = selection s
      in case l of [D i]      -> statify (sub' i) s
                   [V a, V b] -> statify (add' a b) s
                   _          -> s
    handleEdit _ s = s

statify :: (PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r) -> State s v e f r -> State s v e f r
statify f st = st {subdivision = f $ subdivision st}


instance Default Bool where def = True
  -- ^ bad!

sub' :: (Default v, Show v, Show e, Show f, Show r, RealFrac r) => Dart s -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
sub' i d = let (a, b) = d & endPoints i
               p = average [d ^. locationOf a, d ^. locationOf b]
               v = def
               f x = (x, x)
           in splitEdge a b p v f d


add' :: (Default e, Show v, Show e, Show f, Show r) => VertexId' s -> VertexId' s -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
add' a b d | a == b = d
           | length (commonDarts d a b) > 0 = d
           | length (commonFaces d a b) == 0 = d
           | otherwise = let f x = (x, x)
                         in splitFace a b (def, def) f d




{-
tracing :: (HasPSD v e f r s a, Hoverable s a) => GlossApp a -> GlossApp a
tracing = undefined -- output stuff to stdout
-}

tracing :: (Show v, Show e, Show f, Show r, RealFrac r) => App s v e f r -> App s v e f r
tracing app = app
  { handleEvent = \e -> handleTrace e . handleEvent app e
  }
  where
    handleTrace (EventMotion (x, y)) s =
      let t = hoverChanged s
          h = hover s
      in if not t then s else trace (info (subdivision s) h) s {hoverChanged = False}
    handleTrace _ s = s

info :: (Num r, Ord r, Show r, Show v, Show e, Show f) => PlanarSubdivision s v e f r -> PSE s -> String
info _ N = ""
info psd (V i) = "VERTEX " ++ show i ++ "\n"
              ++ "Data: " ++ show (psd ^. dataOf i) ++ "\n"
              ++ "Location: " ++ show (psd ^. locationOf i) ++ "\n"
              ++ "Degree: " ++ show (degree psd i) ++ "\n"
              ++ "Incident edges: " ++ show (incidentEdges i psd) ++ "\n"
              ++ "Adjacent vertices: " ++ show (neighboursOf i psd) ++ "\n"
info psd (D i) = "EDGE " ++ show i ++ "\n"
              ++ "Data: " ++ show (psd ^. dataOf i) ++ "\n"
              ++ "Twin: " ++ show (twin i) ++ "\n"
              ++ "nextIncidentEdge: " ++ show (nextIncidentEdge i psd) ++ "\n"
              ++ "Next on curve: " ++ show (oppositeIncidentEdge psd i) ++ "\n"
              ++ "Incident vertices: " ++ show (endPoints i psd) ++ "\n"
              ++ "Incident faces: " ++ show (leftFace i psd, rightFace i psd) ++ "\n"
info psd (F i) = "FACE " ++ show i ++ "\n"
              ++ "Data: " ++ show (psd ^. dataOf i) ++ "\n"
              ++ "Outer: " ++ show (outerFaceId psd == i) ++ "\n"
              ++ "Boundary: " ++ show (boundaryVertices i psd) ++ "\n"
              ++ "Holes: " ++ show (toList $ holesOf i psd) ++ "\n"

-- ^ slowing down?


{- -- for tracing is the test whether something changed is important!
hoverFunc :: Float -> Float -> State -> State
hoverFunc x y st = 
  let l = locate (subdivision st) (Point2 x y)
  in (if l == hoverElement st then id else trace (info (subdivision st) l)) st {hoverElement = l}
-}

{-
basic :: PlanarSubdivision Whatever Bool () () Float -> GlossApp 
annotated :: GlossApp -> GlossApp 
editable :: GlossApp -> GlossApp 
forcedirected :: GlossApp -> GlossApp 
run :: GlossApp -> IO ()


annotated app = app {draw psd = Pictures [draw psd, ... annotation] }
editable app = 
-}



main :: IO ()
main = run ( id
--           . tracing 
           . forcedirected (simpleForces +++ curvedForces) 
           . editable 
           . selectable 
           . hoverable . hoverableCurves
--           . annotVisible 
           . basicVisible
           ) $ fixall $ forceful vierkant' 

fixall psd = foldr ($) psd $ map (\i -> dataOf i . fluid .~ Fixed (traverse %~ realToFrac $ psd ^. locationOf i)) $ map fst $ toList $ vertices psd

vierkant' :: PlanarSubdivision Whatever Bool () () Rational
vierkant' = fromPolygon (Identity Whatever) vierkant () ()


vierkant = SimplePolygon . C.fromList  $ [ Point2 (-250) (-250) :+ False
--                                         , Point2 0 (-250) :+ True
                                         , Point2 250 (-250) :+ False
--                                         , Point2 250 0 :+ True
                                         , Point2 250 250 :+ False
--                                         , Point2 0 250 :+ True
                                         , Point2 (-250) 250 :+ False
--                                         , Point2 (-250) 0 :+ True

                                         ]


