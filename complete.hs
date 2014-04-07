import Data.List
import System.IO
import Data.Char
import Language.Haskell.TH

type Size = Int -- natural numbers

data Circuit
    = Id Size
    | Fan Size
    | Beside Circuit Circuit
    | Stretch [Size] Circuit
    | Above Circuit Circuit
    deriving(Show, Eq, Ord)

type Layout = [Layer]
type Layer = [Connection] 
type Connection = (Size, Size)

type String' = Char


-- Question 1
width, depth :: Circuit -> Size
width cir = case cir of
   (Id i) -> count i
   (Fan i) -> count i
   (Beside i j) -> (count (width i)) + (count (width j))
   (Stretch xs cir) -> (sum xs)
   (Above i _) -> count (width i)
   where
       count i = i 

depth cir = case cir of
   (Above i j) -> (count' (depth i)) + (count' (depth j))
   _ -> 1
   where
       count' i = i

-- Question 2

-- Question 5
scan :: Size -> Circuit
scan 0 = error "Scan must be positive integer"  
scan 1 = Id 1
scan 2 = Fan 2
scan s = scan' s 1 s
    where
    asc a = a + 1
    desc b = b - 1 
    list' s a b = b:[(x `mod` x)+1 | x <- [1..a]]
    scan' s a b = if b > 2 
        then Stretch (list' s a (desc b)) (Fan (asc a)) `Above`  scan' s (asc a) (desc b) 
        else Stretch [(x `mod` x)+1 | x <- [1..s]] (Fan s)

-- Question 6
serial :: Size -> Circuit
serial 0 = error "Serial param must be an int greater than 0"
serial 1 = Id 1
serial 2 = Fan 2
serial s = fst s (serial' s 0 s)
    where
    fst s cir = (Fan 2) `Beside` (Id (s - 2)) `Above` cir
    lst s = Id (s - 2) `Beside` Fan 2 
    asc a = a + 1
    desc b = b - 3
    serial' s a b = if b > 3 -- one for fst and one for lst
        then (Id (asc a)) `Beside` (Fan 2) `Beside` Id (desc b) `Above` serial' s (asc a) (b - 1) 
        else lst s


-- Question 13
zipCir :: Circuit -> [[Circuit]]
zipCir cir = case cir of
    (Above a b) -> (zipCir a) ++ (zipCir b)
    (_) -> (layer cir) : []
    where
        layer l = case l of
            (Beside a b) -> (layer a) ++ (layer b)
            (x) -> x : []

zipList :: [Int] -> [[Circuit]] -> [[(Int, Circuit)]]
zipList (y:yx) (xs:xss) = [((y + (extract x) - 2),(x)) | x <- xs] : zipList yx xss 
zipList _ [] = []
extract x = case x of
    (Id x) -> x
    (Fan x) -> x
    (Stretch xs x) -> length xs

layout :: Circuit -> Layout
layout cir = pack(zipList [0..] (zipCir cir)) 
    where
        pack(xs:xss) = [package x | x <- xs, filterId x] : (pack xss)
        pack(_) = []
        filterId (_,Id _) = False
        filterId _ = True
        package x = case x of
            (i, (Fan f)) -> (i, (i + f - 1))
            (i, (Stretch ys f)) -> ((head ys) -1, (sum ys)-1)


-- Question 14

svg :: (Layout , Size) -> [String']
svg (lx, s) = concat [createHeader (s - 1), concat(createLine s s), concat(createPoint lx 0), concat(createFan lx 0), footer]
    where
        header = "<svg width='$20' height='$20' viewBox='-10,-10,$20,$20' xmlns='http://www.w3.org/2000/svg' version='1.1'> \n"
        line = "<line x1='$00' y1='0' x2='$00' y2='£00' " ++ style ++ "/> \n"
        fan = "<line x1='α00' y1='β00' x2='ɣ00' y2='ƍ00' " ++ style ++ "/> \n" -- Pattern match on Greek symbols
        point = "<circle cx='$00' cy='£00' r='7' fill='black' stroke-width='0'/>\n"
        style = " stroke='black' stroke-width='2' "
        footer = "</svg> "
        replace _ _ [] = []
        replace a b (x:xs)
            | x == a = b:replace a b xs
            | otherwise = x:replace a b xs
        createHeader s = (replace '$' (intToDigit s) header) 
        createLine i c 
            | i >= 0 = (replace '$' (intToDigit i) (replace '£' (intToDigit c) line)) : createLine (i-1) c
            | otherwise = []
        createPoint (xs:xss) i = if length xs > 1 
            then point' xs i : point'' xs i  : createPoint ((tail xs):xss) i
                else point' xs i : point'' xs i  : createPoint xss (i + 1)
        createPoint [] _ = []
        -- Takes list and int, extracts fst val of tuple, replaces $ for int val, point'' works on snd val of tuple where i is layer
        point' xs i = (replace '$' (intToDigit(fst(head xs))) (replace '£' (intToDigit i) point))
        point'' xs i = (replace '$' (intToDigit(snd(head xs))) (replace '£' (intToDigit (i + 1)) point))
        createFan (xs:xss) i = if length xs > 1
            then fan' xs i : createFan ((tail xs):xss) i
                else fan' xs i : createFan xss (i + 1)
        createFan [] _ = []
        -- Takes list and int and subs out greek letters for values, these are nested, working through tuple vals and layer i val
        fan' xs i = (replace 'α' (intToDigit(fst(head xs))) (replace 'β' (intToDigit i) (replace 'ɣ' (intToDigit(snd(head xs))) (replace 'ƍ' (intToDigit (i + 1)) fan))))

output :: FilePath -> Circuit -> IO()
output file c = writeFile file (unlines([svg(layout c, width c)]))

createFile (xs, s) = do
    writeFile "example2.svg" (svg (xs, s))
    putStr "Done \n"

