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

A more elegant solution
*Main> filter(== Fan 2) [(Fan 2),(Fan 2)]
[Fan 2,Fan 2]
*Main> width(head(filter(== Fan 2) [(Fan 2),(Fan 2)]))
2

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

> depth cir = case cir of
>    (Id i) -> 1
>    (Fan f) -> 1
>    (Beside _ _) -> 1
>    (Above _ _ ) -> 2
>    (Stretch (x:xs) c) -> head xs -- TODO clarify elem in stretch list which is depth

*Main> depth(Id 2)
1
*Main> depth(Fan 2)
1
*Main> depth(Above (Fan 2) (Fan 2))
2
*Main> depth(Beside (Fan 2) (Fan 2))
1
*Main> depth(Stretch[2,3] (Fan 2))
3

Something more elegant...

*Main> length (filter(== Fan 2) [(Fan 2),(Fan 2)])
2

============================
Task 2 
============================

> wellsized :: Circuit -> Bool
> wellsized cir = (width cir) == (depth cir)

*Main> wellsized(Id 1)
True

But this is too easy... 

> wellsized :: Circuit -> Bool
> wellsized cir = case cir of
>    (Id _) -> True
>    (Fan _) -> True
>    (Beside a b) -> (wellsized a) && (wellsized b)
>    (Stretch xs cir) -> length xs >= 2 && (wellsized cir)
>    (Above a b) ->  (wellsized a) && (wellsized b)

the next idea explores the ambiguity of the term ‘well-sized’ so its take to mean that the stretch list values match the corresponding Fan size and that the width of above and below circuits match.

> wellsized :: Circuit -> Bool
> wellsized cir = case cir of
>    (Id i) -> compare' i i
>    (Fan f) -> compare' f f
>    (Beside a b) -> (compare' (wellsized a) (wellsized b))
>    (Stretch xs cir) -> length xs >= 2 && (wellsized cir)
>    (Above a b) ->  (compare' (wellsized a) (wellsized b))
>    where
>        compare' a b = a == b 




============================
Task 9 
============================
> group :: [Int] -> [a] -> [[a]]
> group [] _ = []
> group _ [] = []
> group (x:xs) ys = (take x ys) : (group xs (drop x ys))

*Main> group [1,3,5] "This is a string"
["T","his"," is a"]