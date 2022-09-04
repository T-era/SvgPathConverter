module DScaler (dScale) where

import DParser
import DCommands

dScale :: Double -> PathD -> PathD
dScale rate (PathD pathDItems) = PathD (_dScale rate pathDItems)

_dScale :: Double -> [PathDItem] -> [PathDItem]
_dScale _ [] = []
_dScale rate (Z:ls) = Z:_dScale rate ls
_dScale rate (Move abs pos:ls) = (Move abs (dPosScale rate pos):_dScale rate ls)
_dScale rate (Line abs pos:ls) = (Line abs (dPosScale rate pos):_dScale rate ls)
_dScale rate (Hor abs x:ls) = (Hor abs (rate * x):_dScale rate ls)
_dScale rate (Vert abs y:ls) = (Vert abs (rate * y):_dScale rate ls)
_dScale rate (Curve abs p1 p2 pos:ls) = (Curve abs (dPosScale rate p1) (dPosScale rate p2) (dPosScale rate pos):_dScale rate ls)
_dScale rate (Scurve abs p1 pos:ls) = (Scurve abs (dPosScale rate p1) (dPosScale rate pos):_dScale rate ls)
_dScale rate (Quartic abs p1 pos:ls) = (Quartic abs (dPosScale rate p1) (dPosScale rate pos):_dScale rate ls)
_dScale rate (Turn abs pos:ls) = (Turn abs (dPosScale rate pos):_dScale rate ls)
_dScale rate (Arc abs radius t f1 f2 pos:ls) = (Arc abs (dPosScale rate radius) t f1 f2 (dPosScale rate pos):_dScale rate ls)


dPosScale :: Double -> Pos -> Pos
dPosScale rate (Pos x y) = Pos (rate * x) (rate * y)
