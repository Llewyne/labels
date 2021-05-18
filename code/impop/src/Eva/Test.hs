module Eva.Test where

import Nonogram
import Data.Geometry hiding (head,direction,init,replicate,unit, _direction)
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
frameTest = fromPoints [(Point2 128 (-128)) :+ (),(Point2 (-128) (-128)) :+ (),(Point2 (-128) (128)) :+ (),(Point2 128 (128)) :+ ()] :: SimplePolygon () Float

caseA = [([p6],[6,6,6]),([p1],[1,1,1,1])] :: [UnplacedLabel]
caseB = [([p3],[3,3]),([p2],[2]),([p1],[1])] :: [UnplacedLabel]
caseC = [([p5],[5]),([p4],[4,4,4]),([p3],[3,3]),([p2],[2]),([p1],[1])] :: [UnplacedLabel]
caseE = [([p6],[6,6]),([p3],[3,3]),([p2],[2]),([p1],[1])] :: [UnplacedLabel]

a = Label {_clue = [0], _port = Port {_location = Point2 (100.184) (-128.0), _direction = Vector2 (-0.14785302) (-0.9890093), _side = False}, _offset = 0.0}

caseABox :: [UnplacedLabel]
caseABox = [
     ([Port {_location = Point2 (-128.0) (-70.1027), _direction = Vector2 (-0.6361346) (-0.7715781), _side = True},Port {_location = Point2 (35.3276) (128.0), _direction = Vector2 (0.6361346) (0.771578), _side = False}],[0])
    ,([Port {_location = Point2 (-128.0) (-53.4049), _direction = Vector2 (-0.5963863) (-0.80269754), _side = True},Port {_location = Point2 (6.7798004) (128.0), _direction = Vector2 (0.5963863) (0.8026976), _side = False}],[0])
    ,([Port {_location = Point2 (-96.6386) (128.0), _direction = Vector2 (-0.36783025) (0.9298929), _side = True},Port {_location = Point2 (4.6252503) (-128.0), _direction = Vector2 (0.36783013) (-0.92989296), _side = False}],[0])
    ,([Port {_location = Point2 (4.6252503) (-128.0), _direction = Vector2 (0.36783013) (-0.92989296), _side = True},Port {_location = Point2 (-96.6386) (128.0), _direction = Vector2 (-0.36783025) (0.9298929), _side = False}],[0])
    ,([Port {_location = Point2 (6.7798004) (128.0), _direction = Vector2 (0.5963863) (0.8026976), _side = True},Port {_location = Point2 (-128.0) (-53.4049), _direction = Vector2 (-0.5963863) (-0.80269754), _side = False}],[0])
    ,([Port {_location = Point2 (-54.6249) (128.0), _direction = Vector2 (-0.5129494) (0.8584188), _side = True},Port {_location = Point2 (98.3482) (-128.0), _direction = Vector2 (0.5129492) (-0.85841894), _side = False}],[0])
    ,([Port {_location = Point2 (35.3276) (128.0), _direction = Vector2 (0.6361346) (0.771578), _side = True},Port {_location = Point2 (-128.0) (-70.1027), _direction = Vector2 (-0.6361346) (-0.7715781), _side = False}],[0])
    ,([Port {_location = Point2 (-49.2385) (128.0), _direction = Vector2 (-0.5982082) (0.80134076), _side = True},Port {_location = Point2 (128.0) (-109.423), _direction = Vector2 (0.5982084) (-0.8013405), _side = False}],[0])
    ,([Port {_location = Point2 (-48.6999) (-128.0), _direction = Vector2 (-0.8971849) (-0.44165522), _side = True},Port {_location = Point2 (128.0) (-41.0163), _direction = Vector2 (0.8971848) (0.44165537), _side = False}],[0])
    ,([Port {_location = Point2 (98.3482) (-128.0), _direction = Vector2 (0.5129492) (-0.85841894), _side = True},Port {_location = Point2 (-54.6249) (128.0), _direction = Vector2 (-0.5129494) (0.8584188), _side = False}],[0])
    ,([Port {_location = Point2 (128.0) (-109.423), _direction = Vector2 (0.5982084) (-0.8013405), _side = True},Port {_location = Point2 (-49.2385) (128.0), _direction = Vector2 (-0.5982082) (0.80134076), _side = False}],[0])
    ,([Port {_location = Point2 (128.0) (-41.0163), _direction = Vector2 (0.8971848) (0.44165537), _side = True},Port {_location = Point2 (-48.6999) (-128.0), _direction = Vector2 (-0.8971849) (-0.44165522), _side = False}],[0])]

complicatedFrame =[
    ClosedLineSegment ((Point2 (-128.0) (-119.0)) :+ ()) ((Point2 (-128.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-128.0)) :+ ()) ((Point2 (-109.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-109.0) (-128.0)) :+ ()) ((Point2 (-109.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-109.0) (-128.0)) :+ ()) ((Point2 (-109.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-109.0) (-128.0)) :+ ()) ((Point2 (-63.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-63.0) (-128.0)) :+ ()) ((Point2 (-63.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-63.0) (-128.0)) :+ ()) ((Point2 (-63.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-63.0) (-128.0)) :+ ()) ((Point2 (-39.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-39.0) (-128.0)) :+ ()) ((Point2 (-39.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-39.0) (-128.0)) :+ ()) ((Point2 (-39.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-39.0) (-128.0)) :+ ()) ((Point2 (-34.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-34.0) (-128.0)) :+ ()) ((Point2 (-34.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-34.0) (-128.0)) :+ ()) ((Point2 (-34.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-34.0) (-128.0)) :+ ()) ((Point2 (-24.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-24.0) (-128.0)) :+ ()) ((Point2 (-24.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-24.0) (-128.0)) :+ ()) ((Point2 (-24.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-24.0) (-128.0)) :+ ()) ((Point2 (-3.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-3.0) (-128.0)) :+ ()) ((Point2 (-3.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-3.0) (-128.0)) :+ ()) ((Point2 (-3.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-3.0) (-128.0)) :+ ()) ((Point2 (28.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (28.0) (-128.0)) :+ ()) ((Point2 (28.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (28.0) (-128.0)) :+ ()) ((Point2 (28.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (28.0) (-128.0)) :+ ()) ((Point2 (47.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (47.0) (-128.0)) :+ ()) ((Point2 (47.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (47.0) (-128.0)) :+ ()) ((Point2 (47.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (47.0) (-128.0)) :+ ()) ((Point2 (48.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (48.0) (-128.0)) :+ ()) ((Point2 (48.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (48.0) (-128.0)) :+ ()) ((Point2 (48.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (48.0) (-128.0)) :+ ()) ((Point2 (66.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (66.0) (-128.0)) :+ ()) ((Point2 (66.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (66.0) (-128.0)) :+ ()) ((Point2 (66.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (66.0) (-128.0)) :+ ()) ((Point2 (78.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (78.0) (-128.0)) :+ ()) ((Point2 (78.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (78.0) (-128.0)) :+ ()) ((Point2 (78.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (78.0) (-128.0)) :+ ()) ((Point2 (87.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (87.0) (-128.0)) :+ ()) ((Point2 (87.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (87.0) (-128.0)) :+ ()) ((Point2 (87.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (87.0) (-128.0)) :+ ()) ((Point2 (128.0) (-128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (128.0) (-128.0)) :+ ()) ((Point2 (128.0) (128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (128.0) (128.0)) :+ ()) ((Point2 (-128.0) (128.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (128.0)) :+ ()) ((Point2 (-128.0) (56.0)) :+ ()),
    ClosedLineSegment ((Point2 (-128.0) (56.0)) :+ ()) ((Point2 (-128.0) (51.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (51.0)) :+ ()) ((Point2 (-128.0) (51.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (51.0)) :+ ()) ((Point2 (-128.0) (51.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (51.0)) :+ ()) ((Point2 (-128.0) (31.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (31.0)) :+ ()) ((Point2 (-128.0) (31.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (31.0)) :+ ()) ((Point2 (-128.0) (31.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (31.0)) :+ ()) ((Point2 (-128.0) (2.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (2.0)) :+ ()) ((Point2 (-128.0) (2.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (2.0)) :+ ()) ((Point2 (-128.0) (2.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (2.0)) :+ ()) ((Point2 (-128.0) (-25.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-25.0)) :+ ()) ((Point2 (-128.0) (-25.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-25.0)) :+ ()) ((Point2 (-128.0) (-25.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-25.0)) :+ ()) ((Point2 (-128.0) (-34.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-34.0)) :+ ()) ((Point2 (-128.0) (-34.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-34.0)) :+ ()) ((Point2 (-128.0) (-34.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-34.0)) :+ ()) ((Point2 (-128.0) (-44.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-44.0)) :+ ()) ((Point2 (-128.0) (-44.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-44.0)) :+ ()) ((Point2 (-128.0) (-44.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-44.0)) :+ ()) ((Point2 (-128.0) (-56.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-56.0)) :+ ()) ((Point2 (-128.0) (-56.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-56.0)) :+ ()) ((Point2 (-128.0) (-56.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-56.0)) :+ ()) ((Point2 (-128.0) (-64.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-64.0)) :+ ()) ((Point2 (-128.0) (-64.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-64.0)) :+ ()) ((Point2 (-128.0) (-64.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-64.0)) :+ ()) ((Point2 (-128.0) (-68.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-68.0)) :+ ()) ((Point2 (-128.0) (-68.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-68.0)) :+ ()) ((Point2 (-128.0) (-68.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-68.0)) :+ ()) ((Point2 (-128.0) (-81.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-81.0)) :+ ()) ((Point2 (-128.0) (-81.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-81.0)) :+ ()) ((Point2 (-128.0) (-81.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-81.0)) :+ ()) ((Point2 (-128.0) (-92.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-92.0)) :+ ()) ((Point2 (-128.0) (-91.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-91.0)) :+ ()) ((Point2 (-128.0) (-103.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-103.0)) :+ ()) ((Point2 (-128.0) (-103.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-103.0)) :+ ()) ((Point2 (-128.0) (-103.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-103.0)) :+ ()) ((Point2 (-128.0) (-119.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-119.0)) :+ ()) ((Point2 (-128.0) (-119.0)) :+ ()),	
    ClosedLineSegment ((Point2 (-128.0) (-119.0)) :+ ()) ((Point2 (-128.0) (-119.0)) :+ ())] :: [LineSegment 2 () Float]

complFrameOrig :: SimplePolygon () Float
complFrameOrig = fromPoints [(Point2 (-127.680756) (-118.58617)) :+ (),(Point2 (-127.680855) (-127.680855)) :+ (),(Point2 (-109.12295) (-127.680885)) :+ (),(Point2 (-109.186) (-128.0)) :+ (),(Point2 (-109.12295) (-127.680885)) :+ (),(Point2 (-94.39228) (-127.680695)) :+ (),(Point2 (-94.5052) (-128.0)) :+ (),(Point2 (-94.39228) (-127.680695)) :+ (),(Point2 (-63.2311) (-127.68097)) :+ (),(Point2 (-63.2293) (-128.0)) :+ (),(Point2 (-63.2311) (-127.68097)) :+ (),(Point2 (-39.33151) (-127.68096)) :+ (),(Point2 (-39.2937) (-128.0)) :+ (),(Point2 (-39.33151) (-127.68096)) :+ (),(Point2 (-34.35943) (-127.68095)) :+ (),(Point2 (-34.2822) (-128.0)) :+ (),(Point2 (-34.28354) (-127.68091)) :+ (),(Point2 (-24.064842) (-127.68069)) :+ (),(Point2 (-24.0481) (-128.0)) :+ (),(Point2 (-24.064842) (-127.68069)) :+ (),(Point2 (-3.058686) (-127.68059)) :+ (),(Point2 (-3.04122) (-128.0)) :+ (),(Point2 (-3.058686) (-127.68059)) :+ (),(Point2 (28.01966) (-127.68091)) :+ (),(Point2 (27.8313) (-128.0)) :+ (),(Point2 (28.01966) (-127.68091)) :+ (),(Point2 (46.92328) (-127.680855)) :+ (),(Point2 (46.7476) (-128.0)) :+ (),(Point2 (46.92328) (-127.680855)) :+ (),(Point2 (48.04326) (-127.68085)) :+ (),(Point2 (48.0612) (-128.0)) :+ (),(Point2 (48.04326) (-127.68085)) :+ (),(Point2 (66.23009) (-127.68096)) :+ (),(Point2 (66.245804) (-128.0)) :+ (),(Point2 (66.23009) (-127.68096)) :+ (),(Point2 (77.55273) (-127.68085)) :+ (),(Point2 (77.5695) (-128.0)) :+ (),(Point2 (77.55273) (-127.68085)) :+ (),(Point2 (86.51362) (-127.680725)) :+ (),(Point2 (86.7673) (-128.0)) :+ (),(Point2 (86.51362) (-127.680725)) :+ (),(Point2 (128.31914) (-127.680855)) :+ (),(Point2 (128.31914) (128.31914)) :+ (),(Point2 (-127.680855) (128.31914)) :+ (),(Point2 (-127.68084) (55.849575)) :+ (),(Point2 (-128.0) (55.8699)) :+ (),(Point2 (-127.68084) (55.849575)) :+ (),(Point2 (-127.68091) (50.56034)) :+ (),(Point2 (-128.0) (50.7636)) :+ (),(Point2 (-127.68091) (50.56034)) :+ (),(Point2 (-127.68085) (30.775707)) :+ (),(Point2 (-128.0) (30.7439)) :+ (),(Point2 (-127.68085) (30.775707)) :+ (),(Point2 (-127.68091) (2.0314457)) :+ (),(Point2 (-128.0) (2.0499)) :+ (),(Point2 (-127.68091) (2.0314457)) :+ (),(Point2 (-127.68101) (-25.146555)) :+ (),(Point2 (-128.0) (-25.1921)) :+ (),(Point2 (-127.68101) (-25.146555)) :+ (),(Point2 (-127.6809) (-33.537674)) :+ (),(Point2 (-128.0) (-33.5002)) :+ (),(Point2 (-127.6809) (-33.537674)) :+ (),(Point2 (-127.680916) (-43.783386)) :+ (),(Point2 (-128.0) (-44.3406)) :+ (),(Point2 (-127.680916) (-43.783386)) :+ (),(Point2 (-127.68076) (-56.08)) :+ (),(Point2 (-128.0) (-56.0971)) :+ (),(Point2 (-127.68076) (-56.08)) :+ (),(Point2 (-127.680916) (-63.957325)) :+ (),(Point2 (-128.0) (-64.1767)) :+ (),(Point2 (-127.6809) (-64.19141)) :+ (),(Point2 (-127.680954) (-67.83064)) :+ (),(Point2 (-128.0) (-67.8207)) :+ (),(Point2 (-127.680954) (-67.83064)) :+ (),(Point2 (-127.68106) (-81.04908)) :+ (),(Point2 (-128.0) (-81.0419)) :+ (),(Point2 (-127.68106) (-81.04908)) :+ (),(Point2 (-127.680954) (-91.39443)) :+ (),(Point2 (-128.0) (-91.5736)) :+ (),(Point2 (-127.680954) (-91.39443)) :+ (),(Point2 (-127.68074) (-102.88112)) :+ (),(Point2 (-128.0) (-102.984)) :+ (),(Point2 (-127.68074) (-102.88112)) :+ (),(Point2 (-127.680756) (-118.58617)) :+ (),(Point2 (-128.0) (-119.02)) :+ ()]

