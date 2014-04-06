import System.IO
import Data.Char

type Size = Int -- natural numbers

type Layout = [Layer]
type Layer = [Connection] 
type Connection = (Size, Size)

type MyString = Char

data Circuit
    = Id Size
    | Fan Size
    | Beside Circuit Circuit
    | Stretch [Size] Circuit
    | Above Circuit Circuit
    deriving(Show)

header = " <svg width='$20' height='$20' viewBox='-10,-10,$20,$20' xmlns='http://www.w3.org/2000/svg' version='1.1'> \n"
line = " <line x1='$00' y1='0' x2='$00' y2='£00' stroke='black' stroke-width='2'/> \n"
point = "<circle cx='$00' cy='£00' r='7' fill='black' stroke-width='0'/>\n"
footer = " </svg> "

replace _ _ [] = []
replace a b (x:xs)
    | x == a = b:replace a b xs
    | otherwise = x:replace a b xs

createLine i c 
    | i >= 0 = (replace '$' (intToDigit i) (replace '£' (intToDigit c) line)) : createLine (des i) c
    | otherwise = []
    where 
    	des i = i - 1 

createPoint (xs:xss) = (replace '$' (intToDigit(fst(head xs))) (replace '£' (intToDigit(snd(head xs))) point)) : createPoint xss
createPoint [] = []

svg :: (Layout , Size) -> [MyString]
svg (lx, s) = concat [createHeader s, concat(createLine s s), concat(createPoint lx), createFooter]
    where
        createHeader s = (replace '$' (intToDigit s) header) 
        createFooter = footer

createFile (xs, s) = do
	writeFile "example2.svg" (svg (xs, s))
	putStr "Done \n"
