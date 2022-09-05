module DRounder (roundD) where

import DCommands

roundD :: PathD -> PathD
roundD (PathD pathDItem) = (PathD (map _roundD pathDItem))

_roundD :: PathDItem -> PathDItem
_roundD (Move dabs pos) = Move dabs (roundPos pos)
_roundD (Line dabs pos) = Line dabs (roundPos pos)
_roundD (Hor dabs x) = Hor dabs (roundNumber x)
_roundD (Vert dabs y) = Vert dabs (roundNumber y)
_roundD (Curve dabs p1 p2 pos) = Curve dabs (roundPos p1) (roundPos p2) (roundPos pos)
_roundD (Scurve dabs p1 pos) = Scurve dabs (roundPos p1) (roundPos pos)
_roundD (Quartic dabs p1 pos) = Quartic dabs (roundPos p1) (roundPos pos)
_roundD (Turn dabs pos) = Turn dabs (roundPos pos)
_roundD (Arc dabs radius t f1 f2 pos) = Arc dabs (roundPos radius) t f1 f2 (roundPos pos)
_roundD Z = Z

roundPos :: Pos -> Pos
roundPos (Pos x y) = Pos (roundNumber x) (roundNumber y)

roundNumber :: Double -> Double
roundNumber = fromIntegral.round
