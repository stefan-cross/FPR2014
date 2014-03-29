Introduction

A library of functions specifying and manipulating parallel prefix circuits. Prefix depends on an associative binary operator (+). A prefix computation of size n takes a list input [x1, x2, ..., xn] of lenght n and returns the output of the list [x1, x1 + x2, ..., x1 + x2 + ... xn] - the sums of the none empty prefixes of the input list...

The following is an example of a 4 * 3 Brent-Kung circuit

(Fan 2 ‘Beside‘ Fan 2) ‘Above‘ 
Stretch [ 2, 2 ] (Fan 2) ‘Above‘ 
(Id 1 ‘Beside‘ Fan 2 ‘Beside‘ Id 1)

(Fan 2 `Beside` Fan 2) `Above` Stretch [ 2, 2 ] (Fan 2) `Above` (Id 1 `Beside` Fan 2 `Beside` Id 1)

Task 1 - The following function computes the width and the depth of a circuit assuming circuits are properly defined, implying that if one circuit is Above another then they are the same width.

> type Size = Int -- natural numbers

> data Circuit
>     = Id Size
>     | Fan Size
>     | Beside Circuit Circuit
>     | Stretch [Size] Circuit
>     | Above Circuit Circuit

> width, depth :: Circuit -> Size
> width cir = case cir of
>    (Id i) -> count i
>    (Fan i) -> count i
>    (Beside i j) -> (count (width i)) + (count (width j))
>    (Stretch xs cir) -> (sum xs) -- + (width cir) ?
>    _ -> 0
>    where
>        count i = i 
> depth cir = case cir of
>    (Above i j) -> (count' (depth i)) + (count' (depth j))
>    _ -> 1
>    where
>        count' i = i


Task 2 - The wellsized function determines if a circuit is properly sized and that the size constraints on the Above and Stretch combinators are satisifed. The definition takes ... x .. long to run its functionality

> wellsized :: Circuit -> Bool
> wellsized cir = (width cir) == (depth cir)

Task 3 - A single function determining width and the well-sized functionality of the previous two function is demonstrated below.

> --safewidth :: Circuit -> Maybe Size

Task 4 -

Task 9 - The next function takes a list ws of nat numbers and a list xs of elements such that the sum ws = length xs and partitions xs into segments accourding to length ws eg
group [ 3, 2, 3, 2 ] "functional" = [ "fun", "ct", "ion", "al" ]

> group :: [Int] -> [a] -> [[a]]
> group [] _ = []
> group _ [] = []
> group (x:xs) ys = (take x ys) : (group xs (drop x ys))

*Main> group [1,3,5] "This is a string"
["T","his"," is a"]

