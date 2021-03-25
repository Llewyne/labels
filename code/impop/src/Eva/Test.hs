module Eva.Test where

import Nonogram
import Data.Geometry hiding (head,direction,init,replicate,unit)
import Data.Geometry.Polygon
import Data.Ext
import Control.Lens


_line :: Port -> Line 2 Float
_line p = Data.Geometry.Line (_location p) (Nonogram._direction p)

frame_ :: SimplePolygon () Float
frame_ = fromPoints [Point2 (112.0) (-128.0) :+ (),Point2 (128.0) (-128.0) :+ (),Point2 (128.0) (-96.0) :+ (),Point2 (128.0) (128.0) :+ (),Point2 (16.0) (128.0) :+ (),Point2 (-16.0) (128.0) :+ (),Point2 (-128.0) (128.0) :+ (),Point2 (-128.0) (-48.0) :+ (),Point2 (-128.0) (-96.0) :+ (),Point2 (-128.0) (-128.0) :+ ()]

uls :: [UnplacedLabel]
uls = [([Port {_location = Point2 (1.36705) (-128.0), Nonogram._direction = Vector2 (-0.37423944) (-0.92733216), _side = True},Port {_location = Point2 (104.69) (128.0), Nonogram._direction = Vector2 (0.37427685) (0.9273171), _side = False}],[3]),
    ([Port {_location = Point2 (47.8395) (-128.0), Nonogram._direction = Vector2 (0.35125157) (-0.9362811), _side = True},Port {_location = Point2 (-48.2005) (128.0), Nonogram._direction = Vector2 (-0.35125062) (0.9362815), _side = False}],[1]),
    ([Port {_location = Point2 (71.5081) (-128.0), Nonogram._direction = Vector2 (-0.2693775) (-0.9630347), _side = True},Port {_location = Point2 (128.0) (73.9608), Nonogram._direction = Vector2 (0.26937416) (0.9630357), _side = False}],[1]),
    ([Port {_location = Point2 (128.0) (-76.244), Nonogram._direction = Vector2 (0.90954244) (-0.41561103), _side = True},Port {_location = Point2 (-128.0) (40.7337), Nonogram._direction = Vector2 (-0.9095437) (0.41560838), _side = False}],[1]),
    ([Port {_location = Point2 (128.0) (-56.2167), Nonogram._direction = Vector2 (0.6422371) (-0.7665061), _side = True},Port {_location = Point2 (-26.3526) (128.0), Nonogram._direction = Vector2 (-0.64224195) (0.766502), _side = False}],[3]),
    ([Port {_location = Point2 (128.0) (73.9608), Nonogram._direction = Vector2 (0.26937416) (0.9630357), _side = True},Port {_location = Point2 (71.5081) (-128.0), Nonogram._direction = Vector2 (-0.2693775) (-0.9630347), _side = False}],[0]),
    ([Port {_location = Point2 (128.0) (96.264), Nonogram._direction = Vector2 (0.9586338) (0.2846425), _side = True},Port {_location = Point2 (-128.0) (20.2512), Nonogram._direction = Vector2 (-0.95863426) (-0.28464067), _side = False}],[3]),
    ([Port {_location = Point2 (128.0) (113.105), Nonogram._direction = Vector2 (0.9783424) (0.20699327), _side = True},Port {_location = Point2 (-128.0) (58.9404), Nonogram._direction = Vector2 (-0.9783414) (-0.20699772), _side = False}],[1]),
    ([Port {_location = Point2 (104.69) (128.0), Nonogram._direction = Vector2 (0.37427685) (0.9273171), _side = True},Port {_location = Point2 (1.36705) (-128.0), Nonogram._direction = Vector2 (-0.37423944) (-0.92733216), _side = False}],[1]),
    ([Port {_location = Point2 (-26.3526) (128.0), Nonogram._direction = Vector2 (-0.64224195) (0.766502), _side = True},Port {_location = Point2 (128.0) (-56.2167), Nonogram._direction = Vector2 (0.6422371) (-0.7665061), _side = False}],[1]),
    ([Port {_location = Point2 (-48.2005) (128.0), Nonogram._direction = Vector2 (-0.35125062) (0.9362815), _side = True},Port {_location = Point2 (47.8395) (-128.0), Nonogram._direction = Vector2 (0.35125157) (-0.9362811), _side = False}],[3]),
    ([Port {_location = Point2 (-128.0) (58.9404), Nonogram._direction = Vector2 (-0.9783414) (-0.20699772), _side = True},Port {_location = Point2 (128.0) (113.105), Nonogram._direction = Vector2 (0.9783424) (0.20699327), _side = False}],[0]),
    ([Port {_location = Point2 (-128.0) (40.7337), Nonogram._direction = Vector2 (-0.9095437) (0.41560838), _side = True},Port {_location = Point2 (128.0) (-76.244), Nonogram._direction = Vector2 (0.90954244) (-0.41561103), _side = False}],[3]),
    ([Port {_location = Point2 (-128.0) (20.2512), Nonogram._direction = Vector2 (-0.95863426) (-0.28464067), _side = True},Port {_location = Point2 (128.0) (96.264), Nonogram._direction = Vector2 (0.9586338) (0.2846425), _side = False}],[1])]

testPs = [Port {_location = Point2 (1.36705) (-128.0), Nonogram._direction = Vector2 (-0.37423944) (-0.92733216), _side = False},Port {_location = Point2 (71.5081) (-128.0), Nonogram._direction = Vector2 (-0.2693775) (-0.9630347), _side = False},Port {_location = Point2 (47.8395) (-128.0), Nonogram._direction = Vector2 (0.35125157) (-0.9362811), _side = True},Port {_location = Point2 (47.8395) (-128.0), Nonogram._direction = Vector2 (0.35125157) (-0.9362811), _side = False}]

pos :: Point 2 Float
[Port pos1 dir1 s1,Port pos dir s,Port pos2 dir2 s2,Port pos3 dir3 s3] = testPs
m = 0.766502 / (-0.64224195) ::Float
m2 = (-0.9362811)/0.35125157 ::Float
m1 = (-0.92733216) / (-0.37423944) ::Float
b = pos^.yCoord
b2 = pos2^.yCoord


-- cases from nono labeling pdf figure 6
pDummy1 = Port (Point2 0 0) (Vector2 (-1) (1)) False
p1 = Port (Point2 0 0) (Vector2 1 1) True -- Green label
p2 = Port (Point2 1 0) (Vector2 1 1) True -- Red label
p3 = Port (Point2 2 0) (Vector2 1 1) True -- Yellow label
p4 = Port (Point2 28 0) (Vector2 1 1) False -- Blue label c and d
p5 = Port (Point2 32 0) (Vector2 1 1) False -- Pink label
p6 = Port (Point2 110 0) (Vector2 (-1) 1) True -- Blue label a and e 
pDummy2 = Port (Point2 128 0) (Vector2 1 1) True

edgeTest = OpenLineSegment ((Point2 128 0) :+ ()) ((Point2 0 0) :+ ()) :: LineSegment 2 () Float
frameTest = fromPoints [(Point2 128 0) :+ (),(Point2 0 0) :+ (),(Point2 0 (-128)) :+ (),(Point2 128 (-128)) :+ ()] :: SimplePolygon () Float

caseA = [([p6],[6,6,6]),([p1],[1,1,1,1])] :: [UnplacedLabel]
caseB = [([p3],[3,3]),([p2],[2]),([p1],[1])] :: [UnplacedLabel]
caseC = [([p5],[5]),([p4],[4,4,4]),([p3],[3,3]),([p2],[2]),([p1],[1])] :: [UnplacedLabel]
caseE = [([p6],[6,6]),([p3],[3,3]),([p2],[2]),([p1],[1])] :: [UnplacedLabel]
