{-# LANGUAGE TemplateHaskell
#-}

module Test where


import Control.Lens
import Data.Geometry hiding (head, endPoints)
import Graphics.Gloss (Color, red)

import Data.Extra


import Data.HList

import Data.Geometry.Ipe
import Data.Geometry.Triangulation.Draw
import Algorithms.Geometry.DelaunayTriangulation.Naive
import Data.List.NonEmpty as NonEmpty

mainWith                          :: FilePath -> FilePath -> IO ()
mainWith inFile outFile = do
    ePage <- readSinglePageFile inFile
    case ePage of
      Left err                         -> print err
      Right (page :: IpePage Rational) -> case page^..content.traverse._IpeUse of
        []         -> putStrLn "No points found"
        syms@(_:_) -> do
           let pts  = syms&traverse.core %~ (^.symbolPoint)
               pts' = NonEmpty.fromList pts
               dt   = delaunayTriangulation $ pts'
               out  = [iO $ drawTriangulation dt]
           writeIpeFile outFile . singlePageFromContent $ out

--bla :: [Int, Bool]

{-}
andrew = name .=. "Andrew" .*.
         awesomeness .=. 8000 .*.
         glasses .=. True .*.
         emptyRecord  
-}

-- row :: String :. Int :. Float :. String :. Nil
row :: List '[String, Int, Float, String]
row = "Apple" :. 10 :. 0.5 :. "2018/1/1" :. Nil
--row = Nil




data ForceData = ForceData { _fl :: Bool, _ve :: Vector 2 Float }
$(makeLenses ''ForceData)

forceful :: v -> v :+ ForceData
forceful x = x :+ ForceData True (origin .-. origin)

--class Has v ForceData => Forceful v where
fluid :: HasExtra v ForceData => Lens' v Bool
fluid = extra . fl

force :: HasExtra v ForceData => Lens' v (Vector 2 Float)
force = extra . ve









-- another thing v can have


colorful :: v -> v :+ Color
colorful x = x :+ red 

color :: HasExtra v Color => Lens' v Color
color = extra

{-}

data ColorData = ColorData { _co :: Color }
$(makeLenses ''ColorData)

colorful :: v -> v :+ ColorData
colorful x = x :+ ColorData red 

color :: HasExtra v ColorData => Lens' v Color
color = extra . co

-}








-- test

test1 = view fluid $ forceful ()
test2 = view color $ colorful ()
test3 = view fluid $ forceful $ colorful ()
test4 = view fluid $ colorful $ forceful ()
