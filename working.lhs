> type Size = Int -- natural numbers
> data Circuit
>     = Id Size
>     | Fan Size
>     | Above Circuit Circuit
>     | Beside Circuit Circuit
>     | Stretch [Size] Circuit


============================
Task 1,a
============================

> -- number of Ids, Fans and Stretch values
> width, depth :: Circuit -> Size
> width (Id i) = 1
> width (Fan f) = f
> width (Above _ _ ) = 0
> width (Beside _ _) = 1
> width (Stretch (x:xs) c) = x -- TODO clarify elem in stretch list which is length


Testing:
*Main> width(Id 2)
2
*Main> width(Fan 2)
2
*Main> width(Above (Fan 2)(Fan 2))
0
*Main> width(Beside (Id 2)(Fan 2))
1
*Main> width((Stretch[2,3]) (Fan 2))
2

============================
Task 1,b
============================
!!


============================
Task 9 
============================
> group :: [Int] -> [a] -> [[a]]
> group [] _ = []
> group _ [] = []
> group (x:xs) ys = (take x ys) : (group xs (drop x ys))

*Main> group [1,3,5] "This is a string"
["T","his"," is a"]