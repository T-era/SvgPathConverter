module Util (safeLast) where


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast ls = Just (last ls)
