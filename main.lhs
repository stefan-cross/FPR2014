A library of functions specifying and manipulating parallel prefix circuits. Prefix depends on an associative binary operator (+). A prefix computation of size n takes a list input [x1, x2, ..., xn] of lenght n and returns the output of the list [x1, x1 + x2, ..., x1 + x2 + ... xn] - the sums of the none empty prefixes of the input list...



(Fan 2 ‘Beside‘ Fan 2) ‘Above‘ Stretch [ 2, 2 ] (Fan 2) ‘Above‘ (Id 1 ‘Beside‘ Fan 2 ‘Beside‘ Id 1)

> type Size = Int -- natural numbers
> data Circuit
>     = Id Size
>     | Fan Size
>     | Above Circuit Circuit
>     | Beside Circuit Circuit
>     | Stretch [Size] Circuit


> -- number of Ids, Fans and Stretch values, TODO clarify elem in stretch list which is lenght
> width, depth :: Circuit -> Size
> width (Id i) = i
> width (Fan f) = f
> width (Above _ _ ) = 0
> width (Beside _ _) = 0
> width (Stretch (x:xs) c) = x
> depth c = 1
> -- depth c = length filter (== Above) c Above -- count the number of 'Above' statements

> wellsized :: Circuit -> Bool
> wellsized c = True

