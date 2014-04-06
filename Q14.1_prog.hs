import System.IO
import Data.Char

type Size = Int -- natural numbers

type Layout = [Layer]
type Layer = [Connection] 
type Connection = (Size, Size)

type String' = Char

data Circuit
    = Id Size
    | Fan Size
    | Beside Circuit Circuit
    | Stretch [Size] Circuit
    | Above Circuit Circuit
    deriving(Show)

-- SVG constructors
header = "<svg width='$20' height='$20' viewBox='-10,-10,$20,$20' xmlns='http://www.w3.org/2000/svg' version='1.1'> \n"
line = "<line x1='$00' y1='0' x2='$00' y2='£00' " ++ style ++ "/> \n"
fan = "<line x1='α00' y1='β00' x2='ɣ00' y2='ƍ00' " ++ style ++ "/> \n" -- hack with Greek symbols
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
	then fan' xs i : createFan ((drop 1 xs):xss) i
		else fan' xs i : createFan xss (i + 1)
createFan [] _ = []
-- Takes list and int and subs out greek letters for values, these are nested, working through tuple vals and layer i val
fan' xs i = (replace 'α' (intToDigit(fst(head xs))) (replace 'β' (intToDigit i) (replace 'ɣ' (intToDigit(snd(head xs))) (replace 'ƍ' (intToDigit (i + 1)) fan))))

svg :: (Layout , Size) -> [String']
svg (lx, s) = concat [createHeader (s - 1), concat(createLine s s), concat(createPoint lx 0), concat(createFan lx 0), footer]
        

-- output :: MyString -> Circuit -> IO()
-- output file c = writeFile file (unlines(svg(layout c, width c)))

createFile (xs, s) = do
	writeFile "example2.svg" (svg (xs, s))
	putStr "Done \n"
