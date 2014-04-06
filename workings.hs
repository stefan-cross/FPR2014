import System.IO
import Data.Char

line = " <line x1='$00' y1='$00' x2='$00' y2='$00' stroke='black' stroke-width='2'/> \n"

replace _ _ [] = []
replace a b (x:xs)
    | x == a = b:replace a b xs
    | otherwise = x:replace a b xs

createLines lx s 
            | s > 0 = (replace '$' (intToDigit s) line):createLines lx (des s)
            | otherwise = []
            where 
            	des i = i - 1