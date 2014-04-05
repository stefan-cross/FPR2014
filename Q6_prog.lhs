6. Another obvious way is to accumulate values from left to right—for example:

Define a function to generate parallel prefix circuits of this form in arbitrary sizes.

serial :: Size -> Circuit

(This one still has maximal depth, but at least it now has the minimal number
of nodes possible.)

> module Q5_prog where

> type Size = Int -- natural numbers

> data Circuit
>     = Id Size
>     | Fan Size
>     | Beside Circuit Circuit
>     | Stretch [Size] Circuit
>     | Above Circuit Circuit
>     deriving(Show)

> serial :: Size -> Circuit
> serial 0 = error "Serial param must be an int greater than 0"
> serial 1 = Id 1
> serial 2 = Fan 2
> serial s = fst s (serial' s 0 s)
>     where
>     fst s cir = (Fan 2) `Beside` (Id (s - 2)) `Above` cir
>     lst s = Id (s - 2) `Beside` Fan 2 
>     asc a = a + 1
>     desc b = b - 3
>     serial' s a b = if b > 3 -- one for fst and one for lst
>         then (Id (asc a)) `Beside` (Fan 2) `Beside` Id (desc b) `Above` serial' s (asc a) (b - 1) 
>         else lst s


Not the prettiest function ever written but its work as it shoud not, will optimise later. 

*Main> serial 0
*** Exception: Serial param must be an int greter than 0
*Main> serial 1
Id 1
*Main> serial 2
Fan 2
*Main> serial 3
Above (Beside (Fan 2) (Id 1)) (Beside (Id 1) (Fan 2))
*Main> serial 4
Above (Beside (Fan 2) (Id 2)) (Above (Beside (Beside (Id 1) (Fan 2)) (Id 1)) (Beside (Id 2) (Fan 2)))
*Main> serial 5
Above (Beside (Fan 2) (Id 3)) (Above (Beside (Beside (Id 1) (Fan 2)) (Id 2)) (Above (Beside (Beside (Id 2) (Fan 2)) (Id 1)) (Beside (Id 3) (Fan 2))))
*Main> serial 6
Above (Beside (Fan 2) (Id 4)) (Above (Beside (Beside (Id 1) (Fan 2)) (Id 3)) (Above (Beside (Beside (Id 2) (Fan 2)) (Id 2)) (Above (Beside (Beside (Id 3) (Fan 2)) (Id 1)) (Beside (Id 4) (Fan 2)))))


Also validates against previous width, depth and wellsized functions!

*Q1_prog> width (Above (Beside (Fan 2) (Id 4)) (Above (Beside (Beside (Id 1) (Fan 2)) (Id 3)) (Above (Beside (Beside (Id 2) (Fan 2)) (Id 2)) (Above (Beside (Beside (Id 3) (Fan 2)) (Id 1)) (Beside (Id 4) (Fan 2))))))
6

*Q1_prog> depth  (Above (Beside (Fan 2) (Id 4)) (Above (Beside (Beside (Id 1) (Fan 2)) (Id 3)) (Above (Beside (Beside (Id 2) (Fan 2)) (Id 2)) (Above (Beside (Beside (Id 3) (Fan 2)) (Id 1)) (Beside (Id 4) (Fan 2))))))
5

*Q2_prog> wellsized (Above (Beside (Fan 2) (Id 4)) (Above (Beside (Beside (Id 1) (Fan 2)) (Id 3)) (Above (Beside (Beside (Id 2) (Fan 2)) (Id 2)) (Above (Beside (Beside (Id 3) (Fan 2)) (Id 1)) (Beside (Id 4) (Fan 2))))))
True