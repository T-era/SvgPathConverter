module DParser (readD) where

import Data.Char (isDigit)
import Util
import DCommands

readD :: String -> PathD
readD s = (PathD (readDItems [] s))
readDItems :: [PathDItem] -> String -> [PathDItem]
readDItems temp [] = temp
readDItems temp ('\n':ls) = readDItems temp ls
readDItems temp ('\r':ls) = readDItems temp ls
readDItems temp (' ':ls) = readDItems temp ls
readDItems temp str = readDItems (temp ++ [item]) ls
    where
        (item,ls) = readDItem (safeLast temp) str
readDItem :: Maybe PathDItem -> String -> (PathDItem, String)
readDItem _ ('M':ls) = (Move True pos,lls)
    where (pos, lls) = readDPos ls
readDItem _ ('m':ls) = (Move False pos,lls)
    where (pos, lls) = readDPos ls
readDItem _ ('L':ls) = (Line True pos,lls)
    where (pos, lls) = readDPos ls
readDItem _ ('l':ls) = (Line False pos,lls)
    where (pos, lls) = readDPos ls
readDItem _ ('H':ls) = (Hor True d,lls)
    where (d, lls) = readDNumber ls
readDItem _ ('h':ls) = (Hor False d,lls)
    where (d, lls) = readDNumber ls
readDItem _ ('V':ls) = (Vert True d,lls)
    where (d, lls) = readDNumber ls
readDItem _ ('v':ls) = (Vert False d,lls)
    where (d, lls) = readDNumber ls
readDItem _ ('C':ls) = (Curve True pos1 pos2 pos,lls)
    where
        (pos1, l1s) = readDPos ls
        (pos2, l2s) = readDPos (skipDel l1s)
        (pos, lls) = readDPos (skipDel l2s)
readDItem _ ('c':ls) = (Curve False pos1 pos2 pos,lls)
    where
        (pos1, l1s) = readDPos ls
        (pos2, l2s) = readDPos (skipDel l1s)
        (pos, lls) = readDPos (skipDel l2s)
readDItem _ ('S':ls) = (Scurve True pos1 pos,lls)
    where
        (pos1, l1s) = readDPos ls
        (pos, lls) = readDPos (skipDel l1s)
readDItem _ ('s':ls) = (Scurve False pos1 pos,lls)
    where
        (pos1, l1s) = readDPos ls
        (pos, lls) = readDPos (skipDel l1s)
readDItem _ ('Q':ls) = (Quartic True pos1 pos,lls)
    where
        (pos1, l1s) = readDPos ls
        (pos, lls) = readDPos (skipDel l1s)
readDItem _ ('q':ls) = (Quartic False pos1 pos,lls)
    where
        (pos1, l1s) = readDPos ls
        (pos, lls) = readDPos (skipDel l1s)
readDItem _ ('T':ls) = (Turn True pos,lls)
    where
        (pos, lls) = readDPos (skipDel ls)
readDItem _ ('t':ls) = (Turn False pos,lls)
    where
        (pos, lls) = readDPos (skipDel ls)
readDItem _ ('A':ls) = (Arc True radius t f1 f2 pos,lls)
    where
        (radius, l1s) = readDPos (skipDel ls)
        (t, l2s) = readDNumber (skipDel l1s)
        (f1, l3s) = readDFlag (skipDel l2s)
        (f2, l4s) = readDFlag (skipDel l3s)
        (pos, lls) = readDPos (skipDel l4s)
readDItem _ ('a':ls) = (Arc False radius t f1 f2 pos,lls)
    where
        (radius, l1s) = readDPos (skipDel ls)
        (t, l2s) = readDNumber (skipDel l1s)
        (f1, l3s) = readDFlag (skipDel l2s)
        (f2, l4s) = readDFlag (skipDel l3s)
        (pos, lls) = readDPos (skipDel l4s)
readDItem _ ('Z':ls) = (Z,ls)
readDItem _ ('z':ls) = (Z,ls)
-- 前のコマンド継続パターン
readDItem prev@(Just _) (',':ls) = readDItem prev ls
readDItem (Just (Move dabs _)) str = (Line dabs pos, lls)
    where (pos, lls) = readDPos str
readDItem (Just (Line dabs _)) str = (Line dabs pos, lls)
    where (pos, lls) = readDPos str
readDItem (Just (Hor dabs _)) str = (Hor dabs d, lls)
    where (d, lls) = readDNumber str
readDItem (Just (Vert dabs _)) str = (Vert dabs d, lls)
    where (d, lls) = readDNumber str
readDItem (Just (Curve dabs _ _ _)) str = (Curve dabs p1 p2 pos, lls)
    where
        (p1, l1s) = readDPos (skipDel str)
        (p2, l2s) = readDPos (skipDel l1s)
        (pos, lls) = readDPos (skipDel l2s)
readDItem (Just (Scurve dabs _ _)) str = (Scurve dabs p1 pos, lls)
    where
        (p1, l1s) = readDPos str
        (pos, lls) = readDPos l1s
readDItem (Just (Quartic dabs _ _)) str = (Quartic dabs p1 pos, lls)
    where
        (p1, l1s) = readDPos (skipDel str)
        (pos, lls) = readDPos (skipDel l1s)
readDItem (Just (Turn dabs _)) str = (Turn dabs pos, lls)
    where
        (pos, lls) = readDPos (skipDel str)
readDItem (Just (Arc dabs _ _ _ _ _)) str = (Arc dabs radius t f1 f2 pos, lls)
    where
        (radius, l1s) = readDPos (skipDel str)
        (t, l2s) = readDNumber (skipDel l1s)
        (f1, l3s) = readDFlag (skipDel l2s)
        (f2, l4s) = readDFlag (skipDel l3s)
        (pos, lls) = readDPos (skipDel l4s)


skipDel (' ':ls) = skipDel ls
skipDel (',':ls) = skipDel ls
skipDel ('\n':ls) = skipDel ls
skipDel str = str

readDPos :: String -> (Pos, String)
readDPos (' ':ls) = readDPos ls
readDPos str = (Pos p1 p2, ls)
    where
        (p1, p1s_) = readDNumber str
        p1s = skipDel p1s_
        (p2, ls) = readDNumber p1s

readDNumber :: String -> (Double, String)
readDNumber (' ':ls) = readDNumber ls
readDNumber ('-':ls) = (0 - d, lls)
    where
        (d,lls) = _readDNumber ls
readDNumber str = _readDNumber str

_readDNumber str@('.':ls) = (dbl, lls)
    where
        [(dbl, lls)] = readsPrec 0 ('0':str) :: [(Double,String)]
_readDNumber str = (dbl, lls)
    where
        [(dbl, lls)] = readsPrec 0 str :: [(Double,String)]

readDFlag :: String -> (Bool, String)
readDFlag ('0':ls) = (False, ls)
readDFlag ('1':ls) = (True, ls)

__test = do
  putStrLn (show (readD "M1 2\n"))
  putStrLn (show (readD "M1 2"))
  putStrLn (show (readD "M1 2L3 4 5 6,7 8Z"))
  putStrLn (show (readD "C1 2 3 4 5 6c11 12 13 14 15 16"))
  putStrLn (show (readD "C1,2,3 4 5 6c11 12 13 14 15 16"))
  putStrLn (show (readD "s1,2,3 4"))
  putStrLn (show (readD "s1,2,3 4 11 12 13 14 21 22 23 24"))
  putStrLn (show (readD "a1,2,3 0,1,4 5"))
