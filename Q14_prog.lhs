14. If you want to produce a picture of a circuit, the layout function above provides the necessary information. (Actually, it’s helpful to have the width too; in principle, if it’s a parallel prefix circuit then you can determine the width from the connections, but that doesn’t work for identity circuits.) For example, you might transform a Layout into SVG format. Here is an SVG file for the Brent–Kung circuit of size 4:

     <svg width="320" height="320" viewBox="-10,-10,320,320" xmlns="http://www.w3.org/2000/svg" version="1.1">
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

> import Q1_prog
> import Q2_prog
> import Q13_prog


> svg :: (Layout , Size) -> [String]
> svg (x:xs) s = 




