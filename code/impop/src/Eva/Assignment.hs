module Eva.Assignment where

import Nonogram

import Eva.NonoUtil

type PortAssignment = (UnplacedLabel,Int) -- Possible assignment where the integer is the index of the assigned port of the unplaced label
type VarAssignment = (Int,PortAssignment) -- Possible port assignment mapped to an index  

getPortVA (_,pa) = getPort pa

