module DConverter (toAbs, toRel) where

import DParser
import DCommands

toAbs :: PathD -> PathD
toAbs (PathD pathDItems)= PathD (_toAbs (Pos 0 0) pathDItems)
toRel :: PathD -> PathD
toRel (PathD pathDItems)= PathD (_toRel (Pos 0 0) pathDItems)

_toAbs :: Pos -> [PathDItem] -> [PathDItem]
_toAbs _ [] = []
_toAbs prev (Z:ls) = (Z):(_toAbs prev ls)
_toAbs (Pos px py) (Hor True x:ls) = (Hor True x):(_toAbs (Pos x py) ls)
_toAbs (Pos px py) (Hor False dx:ls) = (Hor True (px+dx)):(_toAbs (Pos (px+dx) py) ls)
_toAbs (Pos px py) (Vert True y:ls) = (Vert True y):(_toAbs (Pos px y) ls)
_toAbs (Pos px py) (Vert False dy:ls) = (Vert True (py+dy)):(_toAbs (Pos px (py+dy)) ls)
_toAbs prev (dItem:ls) = (nextItem):(_toAbs nextPrev ls)
    where
        nextItem = if dabs dItem
            then dItem
            else toAbsItem prev dItem
        nextPrev = pos nextItem

toAbsItem :: Pos -> PathDItem -> PathDItem
toAbsItem prev (Move False pos) = Move True (addPos prev pos)
toAbsItem prev (Line False pos) = Line True (addPos prev pos)
toAbsItem prev (Curve False p1 p2 pos) = Curve True (addPos prev p1) (addPos prev p2) (addPos prev pos)
toAbsItem prev (Scurve False p1 pos) = Scurve True (addPos prev p1) (addPos prev pos)
toAbsItem prev (Quartic False p1 pos) = Quartic True (addPos prev p1) (addPos prev pos)
toAbsItem prev (Turn False pos) = Turn True (addPos prev pos)
toAbsItem prev (Arc False radius t f1 f2 pos) = Arc True radius t f1 f2 (addPos prev pos)


_toRel :: Pos -> [PathDItem] -> [PathDItem]
_toRel _ [] = []
_toRel prev (Z:ls) = (Z):(_toRel prev ls)
_toRel (Pos px py) (Hor True x:ls) = (Hor False (x-px)):(_toRel (Pos x py) ls)
_toRel (Pos px py) (Hor False dx:ls) = (Hor False dx):(_toRel (Pos (px+dx) py) ls)
_toRel (Pos px py) (Vert True y:ls) = (Vert False (y-py)):(_toRel (Pos px y) ls)
_toRel (Pos px py) (Vert False dy:ls) = (Vert False dy):(_toRel (Pos px (py+dy)) ls)
_toRel prev (dItem:ls) = (nextItem):(_toRel nextPrev ls)
    where
        nextItem = if not (dabs dItem)
            then dItem
            else toRelItem prev dItem
        nextPrev = pos nextItem

toRelItem :: Pos -> PathDItem -> PathDItem
toRelItem prev (Move True pos) = Move False (subPos prev pos)
toRelItem prev (Line True pos) = Line False (subPos prev pos)
toRelItem prev (Curve True p1 p2 pos) = Curve False (subPos prev p1) (subPos prev p2) (subPos prev pos)
toRelItem prev (Scurve True p1 pos) = Scurve False (subPos prev p1) (subPos prev pos)
toRelItem prev (Quartic True p1 pos) = Quartic False (subPos prev p1) (subPos prev pos)
toRelItem prev (Turn True pos) = Turn False (subPos prev pos)
toRelItem prev (Arc True radius t f1 f2 pos) = Arc False radius t f1 f2 (subPos prev pos)


addPos :: Pos -> Pos -> Pos
addPos (Pos px py) (Pos cx cy) = Pos (px+cx) (py+cy)
subPos :: Pos -> Pos -> Pos
subPos (Pos px py) (Pos cx cy) = Pos (cx-px) (cy-py)


__test = do
  putStrLn$show$toAbs$readD "m 1 10 2 11"
  putStrLn$show$toRel$readD "M 1 10 2 11"
