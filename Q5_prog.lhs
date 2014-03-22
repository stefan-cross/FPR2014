5. One obvious way to construct a parallel prefix circuit is to use lots of fans; for example, here is a naive parallel prefix circuit of size 8:

| | | | | | | |\|
| | | | | | |\|\|
| | | | | |\|\|\|
...

Define a function to compute circuits of this form of arbitrary size. 

scan :: Size → Circuit

(This is the worst possible parallel prefix circuit, in the sense that it has maximal depth and the maximal number of nodes.)


> type Size = Int -- natural numbers

> data Circuit
>     = Id Size
>     | Fan Size
>     | Beside Circuit Circuit
>     | Stretch [Size] Circuit
>     | Above Circuit Circuit
>     deriving(Show)

> scan :: Size -> Circuit
> scan 0 = error "Scan must be positive integer"  
> scan 1 = Id 1
> scan 2 = Fan 2
> scan s = scan' s 1 s
>     where
>     asc a = a + 1
>     desc b = b - 1 
>     list' s a b = b:[(x `mod` x)+1 | x <- [1..a]]
>     scan' s a b = if b > 2 
>         then Stretch (list' s a (desc b)) (Fan (asc a)) `Above`  scan' s (asc a) (desc b) 
>         else Stretch [(x `mod` x)+1 | x <- [1..s]] (Fan s)


Assuming we are looking for the out put to be as follows:

Stretch[3, 1] Fan 2
`Above`
Stretch[2, 1, 1] fan 3
`Above`
Stretch[1, 1, 1, 1] fan 4        

We are in business:

*Main> scan 4
Above (Stretch [3,1] (Fan 2)) (Above (Stretch [2,1,1] (Fan 3)) (Stretch [1,1,1,1] (Fan 4)))
*Main> scan 5
Above (Stretch [4,1] (Fan 2)) (Above (Stretch [3,1,1] (Fan 3)) (Above (Stretch [2,1,1,1] (Fan 4)) (Stretch [1,1,1,1,1] (Fan 5))))