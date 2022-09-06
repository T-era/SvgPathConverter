module DScaler (dScale) where

import DParser
import DCommands

dScale :: Double -> Double -> PathD -> PathD
dScale rateX rateY (PathD pathDItems) = PathD (_dScale rateX rateY pathDItems)

_dScale :: Double -> Double -> [PathDItem] -> [PathDItem]
_dScale _ _ [] = []
_dScale rateX rateY (Z:ls) = Z:_dScale rateX rateY ls
_dScale rateX rateY (Move abs pos:ls) = (Move abs (dPosScale rateX rateY pos):_dScale rateX rateY ls)
_dScale rateX rateY (Line abs pos:ls) = (Line abs (dPosScale rateX rateY pos):_dScale rateX rateY ls)
_dScale rateX rateY (Hor abs x:ls) = (Hor abs (rateX * x):_dScale rateX rateY ls)
_dScale rateX rateY (Vert abs y:ls) = (Vert abs (rateY * y):_dScale rateX rateY ls)
_dScale rateX rateY (Curve abs p1 p2 pos:ls) = (Curve abs (dPosScale rateX rateY p1) (dPosScale rateX rateY p2) (dPosScale rateX rateY pos):_dScale rateX rateY ls)
_dScale rateX rateY (Scurve abs p1 pos:ls) = (Scurve abs (dPosScale rateX rateY p1) (dPosScale rateX rateY pos):_dScale rateX rateY ls)
_dScale rateX rateY (Quartic abs p1 pos:ls) = (Quartic abs (dPosScale rateX rateY p1) (dPosScale rateX rateY pos):_dScale rateX rateY ls)
_dScale rateX rateY (Turn abs pos:ls) = (Turn abs (dPosScale rateX rateY pos):_dScale rateX rateY ls)
_dScale rateX rateY (Arc abs radius t f1 f2 pos:ls) = (Arc abs (dPosScale rateX rateY radius) t f1 f2 (dPosScale rateX rateY pos):_dScale rateX rateY ls)


dPosScale :: Double -> Double -> Pos -> Pos
dPosScale rateX rateY (Pos x y) = Pos (rateX * x) (rateY * y)
