module TestDynamicPSD where


import Control.Lens

import Data.Ext
import Data.Foldable (toList)
import Data.Functor.Identity

import Data.Geometry
import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.ForceDirected
import qualified Data.CircularSeq as C


import PSDGlossApp



main :: IO ()
main = run ( id
           . traceable
           . forcible (simpleForces +++ curvedForces) 
           . editable 
           . selectable 
           . hoverable . hoverableCurves
           . annotVisible 
           . basicVisible
           ) -- $ fixall 
             $ forceful vierkant' 

fixall psd = foldr ($) psd 
           $ map (\i -> dataOf i . fluid .~ Fixed (traverse %~ realToFrac $ psd ^. locationOf i)) 
           $ map fst $ toList $ vertices psd

--fixall psd = psd & vertexData . traverse . fluid .~ Fixed origin

data TestApp = TestApp

vierkant' :: PlanarSubdivision TestApp () () () Rational
vierkant' = fromPolygon (Identity TestApp) vierkant () ()

vierkant = simpleFromPoints . map (:+ ())
         $ [ Point2 (-250) (-250)
--       , Point2 0 (-250)
         , Point2 250 (-250)
--       , Point2 250 0
         , Point2 250 250
--       , Point2 0 250
         , Point2 (-250) 250
--       , Point2 (-250) 0
         ]

