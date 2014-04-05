
import Q1_prog
import Q12_prog
import Data.List

type Layout = [Layer]
type Layer = [Connection] 
type Connection = Maybe (Size, Size)


zipCir :: Circuit -> [[Circuit]]
zipCir cir = case cir of
    (Above a b) -> (zipCir a) ++ (zipCir b)
    (_) -> (layer cir) : []
    where
        layer l = case l of
            (Beside a b) -> (layer a) ++ (layer b)
            (x) -> x : []

zipList :: [Int] -> [[Circuit]] -> [[(Int, Circuit)]]
zipList = \xs yss -> zipList' xs yss
    where
    	zipList' xs [] = []
    	zipList' xs (ys:yss) = zip xs ys : zipList xs yss

layout :: Circuit -> Layout
layout cir = case cir of
    (Above a b) -> (layout a) ++ (layout b)
    (_) -> (layer cir) : []
    where
    	zipped = (zipList [0..] (zipCir cir)) 
        w = width cir
        layer l = case l of
            (Beside a b) -> (layer a) ++ (layer b)
            (_) -> (calc l 0) : [] 
        calc a i = case a of
            (Stretch (xs) f) -> Just ((head xs) -1 , sum(tail(xs)) + 1)
            (Fan f) ->  Just (f, f)
            (Id i) -> Nothing
