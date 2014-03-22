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
> scan s = scan' s
>     where
>     desc s = s - 1 
>     scan' a = if a > 0 
>         then Stretch [a] (Fan a) `Above`  scan'(desc a) 
>         else Stretch [a] (Fan a)




take a size of x
Id x - 1 Fan 2
`Above`
Id x - 2 `Beside` Stretch [x - 2, 2] Fan 3
`Above`
Id x - 3 `Beside` Stretch [x - 3, 3, 2] Fan 4
`Above`
Id x - 4 `Beside` Stretch [x - 4, 4, 3, 2] Fan 5
...


for a size of 4
Id 3 Fan 2
`Above`
Id 2 `Beside` Stretch[2, 1, 1] fan 3
`Above`
Id 1 `Beside` Stretch[1, 1, 1, 1] fan 4

or is it...

Id 3 Fan 2
`Above`
Stretch[2, 1, 1] fan 3
`Above`
Stretch[1, 1, 1, 1] fan 4

or is it...

Stretch[3, 1] Fan 2
`Above`
Stretch[2, 1, 1] fan 3
`Above`
Stretch[1, 1, 1, 1] fan 4

This doesnt seem to be explained however the latter looks easier to code, so will work to this for now...

Tried a simple countdown but this is proving tricky.

Simple implmentation to regain confidence
 scan :: Size -> Circuit
 scan s =  Above (Stretch[s - 1, 1] (Fan s)) (Stretch[s - 1, 1] (Fan s))

*Main> scan 10
Above (Stretch [9,1] (Fan 10)) (Stretch [9,1] (Fan 10))


FINALLY! This example will take a size and decrement the ID value down to 0 putting the Ids `Beside` eachother!

 scan :: Size -> Circuit
 scan s = scan' s
     where
     desc s = s - 1
     scan' a = if a > 0 then Id a `Beside`  scan'(desc a) else Id a 

We can now work from this iterator approach to count down as required and with some trickery stack Stretch and Fan values `Above` eachother as required i.e.
Stretch[3, 1] Fan 2
`Above`
Stretch[2, 1, 1] fan 3
`Above`
Stretch[1, 1, 1, 1] fan 4        