2. Define a function to determine whether a circuit is ‘well-sized’; in particular,
that the size constraints on the Above and Stretch combinators are satisfied. wellsized :: Circuit → Bool
How long does your function take, in terms of the size of the expression representing the circuit?

import Data.Typeable

> type Size = Int -- natural numbers

> data Circuit
>     = Id Size
>     | Fan Size
>     | Beside Circuit Circuit
>     | Stretch [Size] Circuit
>     | Above Circuit Circuit

> wellsized :: Circuit → Bool
> wellsized cir = case cir of
>    (Stretch xs cir) -> length xs >= 2 && isCircuit cir
>    (Above i j) -> isCircuit i && isCircuit j
>        where 
>        isCircuit i = typeOf i == Circuit 
