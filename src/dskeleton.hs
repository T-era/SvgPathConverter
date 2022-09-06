module DSkeleton (dSkeleton) where

import DCommands

dSkeleton :: PathD -> PathD
dSkeleton (PathD pathDItems) = PathD (_dSkeleton pathDItems)

_dSkeleton :: [PathDItem] -> [PathDItem]
_dSkeleton [] = []
_dSkeleton (Z:ls) = Z:_dSkeleton ls
_dSkeleton (move@(Move _ _):ls) = move:_dSkeleton ls
_dSkeleton (hor@(Hor abs x):ls) = hor:_dSkeleton ls
_dSkeleton (vert@(Vert abs y):ls) = vert:_dSkeleton ls
_dSkeleton (cmd:ls) = (Line (dabs cmd) (pos cmd):_dSkeleton ls)
