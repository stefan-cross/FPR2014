4. How would you place a list of circuits side by side? For example, Id w should be the same as w copies of Id 1 placed side by side. Try to use standard Haskell library functions rather than building entirely from scratch. What is the unit of the binary operator Beside?

> module Q4_prog where
> import Q1_prog
> import Q2_prog
> import Q3_prog


> beside' :: [Circuit] -> Circuit
> beside' [] = Id 0
> beside' (x:xs) = x `Beside` (beside' xs)

> beside :: Circuit -> Circuit
> beside (Id i)
>    | i > 0 = Id i `Beside` beside (Id (i - 1))
>    | otherwise = Id 0


*Main> beside' [(Id 1),(Id 1), (Id 1)]
Beside (Id 1) (Beside (Id 1) (Beside (Id 1) (Id 0)))

*Main> beside (Id 4)
Beside (Id 4) (Beside (Id 3) (Beside (Id 2) (Beside (Id 1) (Id 0))))