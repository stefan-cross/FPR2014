2. Define a function to determine whether a circuit is ‘well-sized’; in particular,
that the size constraints on the Above and Stretch combinators are satisfied. 

wellsized :: Circuit → Bool

How long does your function take, in terms of the size of the expression representing the circuit?

The term ‘well-sized’ seems a bit ambigious but its take to mean that the stretch list values match the corresponding Fan size and that the width of above and below circuits match.

> module Q2_prog where

> import Q1_prog

> wellsized :: Circuit -> Bool
> wellsized cir = case cir of
>    (Id i) -> True
>    (Fan f) -> True
>    (Beside a b) -> (compare' (returnSize a) (returnSize b))
>    (Stretch xs cir) -> (length xs == (returnSize cir))
>    (Above a b) ->  (compare' (returnSize a) (returnSize b))
>    where
>        compare' a b = a == b 
>        returnSize cir = case cir of
>            (Id i) -> i
>            (Fan f) -> f
>            (Beside a b) -> (returnSize a) + (returnSize b)
>            (Stretch xs cir) -> (length xs) + (returnSize cir)
>            (Above a _) -> (returnSize a)



Looking good!

*Main> wellsized (Stretch [3, 2, 3 ] (Fan 3)) 
True

*Main> wellsized (Stretch [ 2, 2 ] (Fan 3)) 
False

*Main> wellsized(Id 1 `Above` Id 1)
True

*Main> wellsized(Id 1 `Above` Id 2)
False

*Main> wellsized(Fan 2 `Beside` Fan 2)
True

*Main> wellsized(Id 1 `Beside` Id 1)
True

And this now works for multiple above statements!

*Main> wellsized  ((Fan 2 `Beside` Fan 2) `Above`(Fan 2 `Beside` Fan 2))
True
*Main> wellsized  ((Fan 2 `Beside` Fan 2) `Above` (Fan 2 `Beside` Fan 2) `Above`(Fan 2 `Beside` Fan 2))
True
*Main> wellsized  ((Fan 2 `Beside` Fan 2) `Above` (Fan 2 `Beside` Fan 2) `Above`(Fan 2 `Beside` Fan 3))
False