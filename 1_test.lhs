Simple is better,

Add up the number of ids

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

!Boom! 

*Main> width((Id 1) `Beside` (Id 1))
2
*Main> width((Id 1) `Beside` (Fan 2))
3

!!We are cooking on gas!

*Main> depth(Above (Fan 2) (Fan 2))
2

!!!!Well more like cooking on electricity, getting there... We can process parts of the example but no the entire circuit representation


*Main> width (Fan 2 `Beside` Fan 2)
4
*Main> width(Stretch [ 2, 2 ] (Fan 2))
4
*Main> width(Id 1 `Beside` Fan 2 `Beside` Id 1)
4
*Main> width(Fan 2 `Beside` Fan 2) `Above` Stretch [ 2, 2 ] (Fan 2) `Above` (Id 1 `Beside` Fan 2 `Beside` Id 1)

<interactive>:440:1:
    Couldn't match type `Int' with `Circuit'
    Expected type: Circuit
      Actual type: Size
    In the return type of a call of `width'
    In the first argument of `Above', namely
      `width (Fan 2 `Beside` Fan 2)'
    In the first argument of `Above', namely
      `width (Fan 2 `Beside` Fan 2) `Above` Stretch [2, 2] (Fan 2)'
*Main> 

!!!Also we can only seem to handle prefix above statements:

*Main> depth(Above (Fan 2) (Fan 2))
2
*Main> depth((Id 2) (Fan 2) `Above` (Id 2) (Fan 2))

<interactive>:446:7:
    Couldn't match expected type `Circuit -> Circuit'
                with actual type `Circuit'
    The function `Id' is applied to two arguments,
    but its type `Size -> Circuit' has only one
    In the first argument of `Above', namely `(Id 2) (Fan 2)'
    In the first argument of `depth', namely
      `((Id 2) (Fan 2) `Above` (Id 2) (Fan 2))'

<interactive>:446:30:
    Couldn't match expected type `Circuit -> Circuit'
                with actual type `Circuit'
    The function `Id' is applied to two arguments,
    but its type `Size -> Circuit' has only one
    In the second argument of `Above', namely `(Id 2) (Fan 2)'
    In the first argument of `depth', namely
      `((Id 2) (Fan 2) `Above` (Id 2) (Fan 2))'
*Main> 


