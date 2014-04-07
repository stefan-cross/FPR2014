14. If you want to produce a picture of a circuit, the layout function above provides the necessary information. (Actually, it’s helpful to have the width too; in principle, if it’s a parallel prefix circuit then you can determine the width from the connections, but that doesn’t work for identity circuits.) For example, you might transform a Layout into SVG format. Here is an SVG file for the Brent–Kung circuit of size 4:

     <svg width="320" height="320" viewBox="-10,-10,320,320" xmlns="http://www.w3.org/2000/svg" version="1.1'>
     <line x1="0" y1="0" x2="0" y2="300" stroke="black" stroke-width="2"/>
     <line x1="100" y1="0" x2="100" y2="300" stroke="black" stroke-width="2"/>
     <line x1="200" y1="0" x2="200" y2="300" stroke="black" stroke-width="2"/>
     <line x1="300" y1="0" x2="300" y2="300" stroke="black" stroke-width="2"/>
     <line x1="0" y1="0" x2="100" y2="100" stroke="black" stroke-width="2"/>
     <line x1="200" y1="0" x2="300" y2="100" stroke="black" stroke-width="2"/>
     <line x1="100" y1="100" x2="300" y2="200" stroke="black" stroke-width="2"/>
     <line x1="100" y1="200" x2="200" y2="300" stroke="black" stroke-width="2"/>
     <circle cx="0" cy="0" r="7" fill="black" stroke-width="0"/>
     <circle cx="100" cy="100" r="7" fill="black" stroke-width="0"/>
     <circle cx="100" cy="200" r="7" fill="black" stroke-width="0"/>
     <circle cx="200" cy="0" r="7" fill="black" stroke-width="0"/>
     <circle cx="200" cy="300" r="7" fill="black" stroke-width="0"/>
     <circle cx="300" cy="100" r="7" fill="black" stroke-width="0"/>
     <circle cx="300" cy="200" r="7" fill="black" stroke-width="0"/>
     </svg>

Define a function to produce such a file. 

svg :: (Layout , Size) -> [ String ]

The SVG quoted above should give you enough information to work with, but in case you want more, there is an SVG reference manual at

http://www.w3schools.com/svg/

You can write your generated SVG out to a file with the following function output :: String ! Circuit ! IO ()
output file c = writeFile file (unlines (svg (layout c, width c)))
If you don’t have a dedicated SVG viewer to hand, the Google Chrome browser should suffice. (Essentially such a technique was used to generate the dia- grams in this document, except that I translated to Metapost rather than SVG.)


> module Q14_prog where

> import Data.List
> import System.IO
> import Data.Char
> import Q1_prog
> import Q2_prog
> import Q3_prog
> import Q4_prog
> import Q5_prog
> import Q6_prog
> import Q8_prog
> import Q13_prog

> type String' = Char

> svg :: (Layout , Size) -> [String']
> svg (lx, s) = concat [createHeader (s - 1), concat(createLine s s), concat(createPoint lx 0), concat(createFan lx 0), footer]
>     where
>         header = " <svg width='$20' height='$20' viewBox='-10,-10,$20,$20' xmlns='http://www.w3.org/2000/svg' version='1.1'> \n"
>         line = " <line x1='$00' y1='0' x2='$00' y2='£00' " ++ style ++ "/> \n"
>         fan = " <line x1='α00' y1='β00' x2='ɣ00' y2='ƍ00' " ++ style ++ "/> \n" -- Pattern match on Greek symbols
>         point = " <circle cx='$00' cy='£00' r='7' fill='black' stroke-width='0'/>\n"
>         style = " stroke='black' stroke-width='2' "
>         footer = " </svg> "
>         replace _ _ [] = []
>         replace a b (x:xs)
>             | x == a = b:replace a b xs
>             | otherwise = x:replace a b xs
>         createHeader s = (replace '$' (intToDigit s) header) 
>         createLine i c 
>             | i >= 0 = (replace '$' (intToDigit i) (replace '£' (intToDigit c) line)) : createLine (i-1) c
>             | otherwise = []
>         createPoint (xs:xss) i = if length xs > 1 
>             then point' xs i : point'' xs i  : createPoint ((tail xs):xss) i
>                 else point' xs i : point'' xs i  : createPoint xss (i + 1)
>         createPoint [] _ = []
>         -- Takes list and int, extracts fst val of tuple, replaces '$' for int val, point'' works on snd val of tuple where i is layer
>         point' xs i = (replace '$' (intToDigit(fst(head xs))) (replace '£' (intToDigit i) point))
>         point'' xs i = (replace '$' (intToDigit(snd(head xs))) (replace '£' (intToDigit (i + 1)) point))
>         createFan (xs:xss) i = if length xs > 1
>             then fan' xs i : createFan ((tail xs):xss) i
>                 else fan' xs i : createFan xss (i + 1)
>         createFan [] _ = []
>         -- Takes list and int and subs out greek letters for values, these are nested, working through tuple vals and layer i val
>         fan' xs i = (replace 'α' (intToDigit(fst(head xs))) (replace 'β' (intToDigit i) (replace 'ɣ' (intToDigit(snd(head xs))) (replace 'ƍ' (intToDigit (i + 1)) fan))))

> output :: FilePath -> Circuit -> IO()
> output file c = writeFile file (unlines([svg(layout c, width c)]))



