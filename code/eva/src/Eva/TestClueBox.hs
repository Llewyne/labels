{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module TestClueBox where

import Eva.ClueBox
import Nonogram
import Data.Geometry hiding (_direction, location)
import Data.Geometry.Point
import Control.Lens

----- Test cluebox intersection -----

--- These should not intersect --
ulsCluebox = [([Port {_location = Point2 (-128.0) (-28.0), _direction = Vector2 (-0.9501576) (-0.3117703), _side = True},Port {_location = Point2 (128.0) (56.0), _direction = Vector2 (0.9501574) (0.31177065), _side = False}],[0]),
    ([Port {_location = Point2 (-128.0) (8.0), _direction = Vector2 (-0.8411785) (-0.5407576), _side = True},Port {_location = Point2 (-16.0) (80.0), _direction = Vector2 (0.8411785) (0.5407576), _side = False}],[0]),
    ([Port {_location = Point2 (-128.0) (36.0), _direction = Vector2 (-0.73160744) (0.6817261), _side = True},Port {_location = Point2 (48.0) (-128.0), _direction = Vector2 (0.73160815) (-0.6817253), _side = False}],[1]),
    ([Port {_location = Point2 (-128.0) (120.0), _direction = Vector2 (-0.9417419) (0.33633643), _side = True},Port {_location = Point2 (-16.0) (80.0), _direction = Vector2 (0.9417419) (-0.33633646), _side = False}],[0]),
    ([Port {_location = Point2 (48.0) (-128.0), _direction = Vector2 (0.73160815) (-0.6817253), _side = True},Port {_location = Point2 (-128.0) (36.0), _direction = Vector2 (-0.73160744) (0.6817261), _side = False}],[0]),
    ([Port {_location = Point2 (-16.0) (80.0), _direction = Vector2 (0.8411785) (0.5407576), _side = True},Port {_location = Point2 (-128.0) (8.0), _direction = Vector2 (-0.8411785) (-0.5407576), _side = False}],[0]),
    ([Port {_location = Point2 (128.0) (56.0), _direction = Vector2 (0.9501574) (0.31177065), _side = True},Port {_location = Point2 (-128.0) (-28.0), _direction = Vector2 (-0.9501576) (-0.3117703), _side = False}],[1]),
    ([Port {_location = Point2 (-48.0) (-128.0), _direction = Vector2 (-0.24253498) (-0.97014266), _side = True},Port {_location = Point2 (8.000095) (96.00006), _direction = Vector2 (0.24254453) (0.9701403), _side = False}],[1]),
    ([Port {_location = Point2 (8.000095) (96.00006), _direction = Vector2 (0.24254453) (0.9701403), _side = True},Port {_location = Point2 (-48.0) (-128.0), _direction = Vector2 (-0.24253498) (-0.97014266), _side = False}],[0]),
    ([Port {_location = Point2 (-16.0) (80.0), _direction = Vector2 (0.9417419) (-0.33633646), _side = True},Port {_location = Point2 (-128.0) (120.0), _direction = Vector2 (-0.9417419) (0.33633643), _side = False}],[0]),
    ([Port {_location = Point2 (-16.0) (80.0), _direction = Vector2 (-0.94868344) (0.31622744), _side = True},Port {_location = Point2 (128.0) (32.0), _direction = Vector2 (0.94868326) (-0.31622785), _side = False}],[0]),
    ([Port {_location = Point2 (128.0) (32.0), _direction = Vector2 (0.94868326) (-0.31622785), _side = True},Port {_location = Point2 (-16.0) (80.0), _direction = Vector2 (-0.94868344) (0.31622744), _side = False}],[0]),
    ([Port {_location = Point2 (8.000095) (96.00006), _direction = Vector2 (-0.8320502) (-0.5547004), _side = True},Port {_location = Point2 (56.0) (128.0), _direction = Vector2 (0.83205014) (0.5547004), _side = False}],[2]),
    ([Port {_location = Point2 (8.000095) (96.00006), _direction = Vector2 (-0.24252917) (-0.9701441), _side = True},Port {_location = Point2 (16.0) (128.0), _direction = Vector2 (0.24253555) (0.97014254), _side = False}],[0]),
    ([Port {_location = Point2 (16.0) (128.0), _direction = Vector2 (0.24253555) (0.97014254), _side = True},Port {_location = Point2 (8.000095) (96.00006), _direction = Vector2 (-0.24252917) (-0.9701441), _side = False}],[1]),
    ([Port {_location = Point2 (32.0) (-128.0), _direction = Vector2 (-0.0) (-1.0), _side = True},Port {_location = Point2 (32.0) (128.0), _direction = Vector2 (-0.0) (1.0), _side = False}],[1]),
    ([Port {_location = Point2 (32.0) (128.0), _direction = Vector2 (-0.0) (1.0), _side = True},Port {_location = Point2 (32.0) (-128.0), _direction = Vector2 (-0.0) (-1.0), _side = False}],[1]),
    ([Port {_location = Point2 (56.0) (128.0), _direction = Vector2 (0.83205014) (0.5547004), _side = True},Port {_location = Point2 (8.000095) (96.00006), _direction = Vector2 (-0.8320502) (-0.5547004), _side = False}],[0])]

-- These should have intersections --
ulsCluebox2 = [([Port {_location = Point2 (-128.0) (-52.0), _direction = Vector2 (-0.9664848) (-0.25672373), _side = True},Port {_location = Point2 (128.0) (16.0), _direction = Vector2 (0.9664851) (0.25672266), _side = False}],[0]),
    ([Port {_location = Point2 (-128.0) (-20.0), _direction = Vector2 (-0.8077909) (0.5894692), _side = True},Port {_location = Point2 (20.0) (-128.0), _direction = Vector2 (0.80779284) (-0.58946645), _side = False}],[1]),
    ([Port {_location = Point2 (-128.0) (48.0), _direction = Vector2 (-0.9309732) (-0.36508766), _side = True},Port {_location = Point2 (76.0) (128.0), _direction = Vector2 (0.930973) (0.36508808), _side = False}],[0]),
    ([Port {_location = Point2 (-128.0) (72.0), _direction = Vector2 (-0.98554903) (0.16939062), _side = True},Port {_location = Point2 (128.0) (28.0), _direction = Vector2 (0.98554885) (-0.16939145), _side = False}],[2]),
    ([Port {_location = Point2 (20.0) (-128.0), _direction = Vector2 (0.80779284) (-0.58946645), _side = True},Port {_location = Point2 (-128.0) (-20.0), _direction = Vector2 (-0.8077909) (0.5894692), _side = False}],[0]),
    ([Port {_location = Point2 (128.0) (16.0), _direction = Vector2 (0.9664851) (0.25672266), _side = True},Port {_location = Point2 (-128.0) (-52.0), _direction = Vector2 (-0.9664848) (-0.25672373), _side = False}],[1]),
    ([Port {_location = Point2 (128.0) (28.0), _direction = Vector2 (0.98554885) (-0.16939145), _side = True},Port {_location = Point2 (-128.0) (72.0), _direction = Vector2 (-0.98554903) (0.16939062), _side = False}],[0]),
    ([Port {_location = Point2 (76.0) (128.0), _direction = Vector2 (0.930973) (0.36508808), _side = True},Port {_location = Point2 (-128.0) (48.0), _direction = Vector2 (-0.9309732) (-0.36508766), _side = False}],[1]),
    ([Port {_location = Point2 (-32.0) (128.0), _direction = Vector2 (-0.78086823) (0.6246957), _side = True},Port {_location = Point2 (128.0) (0.0), _direction = Vector2 (0.7808686) (-0.62469536), _side = False}],[0]),
    ([Port {_location = Point2 (-20.0) (-128.0), _direction = Vector2 (-0.12403746) (-0.9922776), _side = True},Port {_location = Point2 (12.0) (128.0), _direction = Vector2 (0.12403473) (0.99227786), _side = False}],[1,1]),
    ([Port {_location = Point2 (12.0) (128.0), _direction = Vector2 (0.12403473) (0.99227786), _side = True},Port {_location = Point2 (-20.0) (-128.0), _direction = Vector2 (-0.12403746) (-0.9922776), _side = False}],[1]),
    ([Port {_location = Point2 (128.0) (0.0), _direction = Vector2 (0.7808686) (-0.62469536), _side = True},Port {_location = Point2 (-32.0) (128.0), _direction = Vector2 (-0.78086823) (0.6246957), _side = False}],[2])]

-- Pairs of ports that should intersect but don't
pair1 = (Port {_location = Point2 (128.0) (28.0), _direction = Vector2 (0.98554885) (-0.16939145), _side = False},Port {_location = Point2 (-128.0) (48.0), _direction = Vector2 (-0.9309732) (-0.36508766), _side = True})

testIntersections :: IO ()
testIntersections = do 
    let ps = concat $ map fst ulsCluebox
    let pairs = [(p1,p2)|p1<-ps,p2<-ps]
    mapM_ testIntersection pairs


cbFromPort (Port p v s) = clueBoxPolygon p v s

testIntersection :: (Port, Port) -> IO ()
testIntersection (p1,p2) = do
    putStrLn ((show $ p1^.location) ++ (show $ p1^.side) ++ " intersection with " ++ (show $ p2^.location) ++ (show $ p2^.side))
    putStrLn (show ((cbFromPort p1) `intersects` (cbFromPort p2)))
    putStrLn "---------------------------------------------------------------------"


-- test other kinds of intersections
p1 = Port {_location = Point2 (-128.0) (-20.0), _direction = Vector2 (-0.8077909) (0.5894692), _side = True}
p2 = Port {_location = Point2 (-128.0) (48.0), _direction = Vector2 (-0.9309732) (-0.36508766), _side = True}
cb1 = cbFromPort p1
cb2 = cbFromPort p2
cbs = [cb1,cb2]