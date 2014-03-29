4. How would you place a list of circuits side by side? For example, Id w should be the same as w copies of Id 1 placed side by side. Try to use standard Haskell library functions rather than building entirely from scratch. What is the unit of the binary operator Beside?

> module Q4_prog where

> type Size = Int -- natural numbers

> data Circuit
>     = Id Size
>     | Fan Size
>     | Beside Circuit Circuit
>     | Stretch [Size] Circuit
>     | Above Circuit Circuit
>     deriving(Show)

> beside :: [Circuit] -> Circuit
> beside [] = Id 1
> beside (x:xs) = x `Beside` (beside xs)

Seems to be along the right lines...

*Main> beside [(Id 1),(Id 1), (Id 1)]
Beside (Id 1) (Beside (Id 1) (Beside (Id 1) (Id 1)))

However we are not handeling the empty list very well here...

Also how do we work with Id values greater then 1, say Id 4, what are the implications of this?

Consider a tuple of (Id, w) and decremeting w 