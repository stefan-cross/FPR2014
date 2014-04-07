3. You should discover that there is some overlap in behaviour (and possibly duplication of code) between width and wellsized. Write a single total function that returns a Maybe value, combining the two: Just (width c) when circuit c is well-sized, and Nothing otherwise. How long does this function take?

> module Q3_prog where
> import Q1_prog
> import Q2_prog

> safewidth :: Circuit -> Maybe Size
> safewidth cir = if (wellsized cir) then Just(width cir) else Nothing

Without revising the previous work we can mash the functionality together as above! There is indeed similarities in approach of the two functions. 

[1 of 3] Compiling Q1_prog          ( Q1_prog.lhs, interpreted )
[2 of 3] Compiling Q2_prog          ( Q2_prog.lhs, interpreted )
[3 of 3] Compiling Q3_prog          ( Q3_prog.lhs, interpreted )
Ok, modules loaded: Q3_prog, Q1_prog, Q2_prog.
*Q3_prog> safewidth (Id 4 `Above` Id 4)
Just 4
*Q3_prog> safewidth (Id 4 `Above` Id 5)
Nothing
*Q3_prog> safewidth ((Fan 2 `Beside` Fan 2) `Above` Stretch [ 2, 2 ] (Fan 2) `Above` (Id 1 `Beside` Fan 2 `Beside` Id 1))
Just 4
*Q3_prog> safewidth ((Fan 2 `Beside` Fan 2) `Above` (Fan 2 `Beside` Fan 2) `Above`(Fan 2 `Beside` Fan 3))
Nothing

Regarding the execution time. The function first check if a circuit is wellsized, the well sized function in turn breaks down the circuit into its seperate parts, comparing the width of circuits to ensure circuits above one another are indeed the same width. If this is the case we then take the circuit and reassess the width of the circuit... I can see that this is very inefficient, however it is working for now, and I will revisit. Perhaps this is to highlight the fact that I havent utilised the width and depth functions in the wellsized function!?

Still its 11:15 and apt for some down time otherwise I cant sleep after thinking recurisvely!