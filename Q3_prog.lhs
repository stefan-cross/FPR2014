3. You should discover that there is some overlap in behaviour (and possibly duplication of code) between width and wellsized. Write a single total func- tion that returns a Maybe value, combining the two: Just (width c) when circuit c is well-sized, and Nothing otherwise. How long does this function take?


> type Size = Int -- natural numbers

> data Circuit
>     = Id Size
>     | Fan Size
>     | Beside Circuit Circuit
>     | Stretch [Size] Circuit
>     | Above Circuit Circuit

> safewidth :: Circuit -> Maybe Size