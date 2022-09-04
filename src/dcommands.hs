module DCommands (PathD(PathD)
    , PathDItem(Move,Line,Hor,Vert,Curve,Scurve,Quartic,Turn,Arc,Z, dabs, pos)
    , Pos(Pos,x,y)) where

import Data.Char (isDigit)
import Util

data Pos = Pos { x :: Double, y :: Double } deriving (Eq)
data PathDItem = Move { dabs :: Bool, pos :: Pos }
    | Line { dabs :: Bool, pos :: Pos }
    | Hor { dabs :: Bool, d :: Double }
    | Vert { dabs :: Bool, d :: Double }
    | Curve { dabs :: Bool, p1 :: Pos, p2 :: Pos, pos :: Pos }
    | Scurve { dabs :: Bool, p1 :: Pos, pos :: Pos }
    | Quartic { dabs :: Bool, p1 ::Pos, pos :: Pos }
    | Turn { dabs :: Bool, pos :: Pos }
    | Arc { dabs :: Bool, radius :: Pos, t :: Double, f1 :: Bool, f2 :: Bool, pos :: Pos}
    | Z
    deriving (Eq)
data PathD = PathD { pathDItems :: [PathDItem] }

cmd :: PathDItem -> Char
cmd (Move True _) = 'M'
cmd (Move False _) = 'm'
cmd (Line True _) = 'L'
cmd (Line False _) = 'l'
cmd (Hor True _) = 'H'
cmd (Hor False _) = 'h'
cmd (Vert True _) = 'V'
cmd (Vert False _) = 'v'
cmd (Curve True _ _ _) = 'C'
cmd (Curve False _ _ _) = 'c'
cmd (Scurve True _ _) = 'S'
cmd (Scurve False _ _) = 's'
cmd (Quartic True _ _) = 'Q'
cmd (Quartic False _ _) = 'q'
cmd (Turn True _) = 'T'
cmd (Turn False _) = 't'
cmd (Arc True _ _ _ _ _) = 'A'
cmd (Arc False _ _ _ _ _) = 'a'
cmd Z = 'Z'

instance Show Pos where
    show (Pos x y) = show x ++ " " ++ show y
instance Show PathDItem where
    show (Move _ pos) = show pos
    show (Line _ pos) = show pos
    show (Hor _ d) = show d
    show (Vert _ d) = show d
    show (Curve _ p1 p2 pos) = show p1 ++ "," ++ show p2 ++ "," ++ show pos
    show (Scurve _ p1 pos) = show p1 ++ "," ++ show pos
    show (Quartic _ p1 pos) = show p1 ++ "," ++ show pos
    show (Turn _ pos) = show pos
    show (Arc _ radius t f1 f2 pos) = show radius ++ " " ++ (show t) ++ " " ++ (showFlag f1) ++ " " ++ (showFlag f2) ++ " " ++ (show pos)
    show Z = ""
showFlag True = "1"
showFlag False = "0"

instance Show PathD where
    show (PathD []) = ""
    show (PathD (pathDItem:[])) = (cmd pathDItem:show pathDItem)
    show (PathD (pathDItem1:pathDItem2:ls)) =
        let
            c1 = cmd pathDItem1
            c2 = cmd pathDItem2
            in
                if c1 == c2
                    then ((cmd pathDItem1:show pathDItem1) ++ "," ++ showContinue (pathDItem2:ls))
                    else ((cmd pathDItem1:show pathDItem1) ++ show (PathD (pathDItem2:ls)))

showContinue [] = ""
showContinue (a:[]) = show a
showContinue (a1:a2:ls) =
    let
        c1 = cmdForContinue a1
        c2 = cmdForContinue a2
        in
            if c1 == c2
                then ((show a1) ++ "," ++ showContinue (a2:ls))
                else ((show a1) ++ show (PathD (a2:ls)))
cmdForContinue :: PathDItem -> Char
cmdForContinue (Move True _) = 'L'
cmdForContinue (Move False _) = 'l'
cmdForContinue item = cmd item
