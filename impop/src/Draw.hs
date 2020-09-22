module Draw where


import Control.Lens

import Data.Foldable (toList)
import Data.Ext

import Graphics.Gloss hiding (Point, Arc)

import Data.Geometry hiding (head, endPoints, edgeSegments)
import Data.Geometry.PlanarSubdivision
import Data.Geometry.Polygon
import Data.PlanarGraph.Dart

import Algorithms.Geometry.Misc

-- a way to draw things showing ids


----

annotate :: Bool
annotate = True

fontsize = 0.1

class Draw a where -- maybe exists already?
  draw :: a -> Picture

-- basic instances

instance Draw Picture where 
  draw = id

instance Draw () where 
  draw () = Blank

instance (Draw a, Draw b) => Draw (a, b) where
  draw (a, b) = Pictures [draw a, draw b]

instance Draw Char where
  draw c = Scale fontsize fontsize $ Translate (-40) (-50) $ Text [c]

instance Draw String where 
  draw cs = let l  = fromInteger $ toInteger $ length cs
                xs = map (\i -> (i - (l + 1) / 2) * 80 * fontsize) [1 .. l]
            in Pictures $ map (\(x, c) -> Translate x 0 $ draw c) $ zip xs cs

-- geometry

instance Draw (VertexId' s) where
  draw i = Pictures [ Color blue $ circleSolid $ 100 * fontsize
                    , Color yellow $ draw $ show $ fromEnum i
                    ]

instance Draw (FaceId' s) where
  draw i = Pictures [ Color green $ circleSolid $ 100 * fontsize
                    , Color yellow $ draw $ show $ fromEnum i
                    ]

instance Draw (Dart s) where
  draw (Dart i c) = Pictures [ Color red $ circleSolid $ 100 * fontsize
                             , Color yellow $ draw $ show (fromEnum i)
                             ]

toFloat :: RealFrac r => Point 2 r -> (Float, Float)
toFloat (Point2 x y) = (realToFrac x, realToFrac y)

drawAt :: (RealFrac r, Draw a) => Point 2 r -> a -> Picture
drawAt p = uncurry Translate (toFloat p) . draw

drawVertex :: (RealFrac r, Draw v) => (VertexId' s, VertexData r v) -> Picture
drawVertex (i, VertexData p v) = Pictures [ drawAt p v, drawAt p i ]

drawEdge :: RealFrac r => (Dart s, LineSegment 2 v r :+ e) -> Picture
drawEdge (d, s :+ e) = let (p :+ _, q :+ _) = orderedEndPoints s 
                       in Pictures [ Graphics.Gloss.Line [toFloat p, toFloat q]
                                   , drawAt (average [p, q]) d
                                   ]


instance (RealFrac r, Draw v, Draw e, Draw f) => Draw (PlanarSubdivision s v e f r) where
  draw psd = Pictures
    $  map drawEdge   (toList $ edgeSegments psd)
    ++ map drawVertex (toList $ vertices psd)



