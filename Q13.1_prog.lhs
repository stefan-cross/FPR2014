13. Suppose that you have to generate an actual circuit layout in some hardware description language, from a term of type Circuit. The essence of the translation is to determine the connections between wires. Note that each circuit can be thought of as a sequence of layers, and connections only go from one layer to the next (and only rightwards, too). So it suffices to generate a list of layers, where each layer is a collection of pairs (i, j) with i < j denoting a connection from wire i on this layer to wire j on the next. The ordering of the pairs on each layer is not significant. We count from 0. 

For example, the Brent–Kung circuit of size 4 given earlier has the following connections:

[[(0, 1), (2, 3)], [(1, 3)], [(1, 2)]]

That is, there are three layers; the first layer has connections from wire 0 to wire 1 and from wire 2 to wire 3; the second a single connection from wire 1 to wire 3; and the third a single connection from wire 1 to wire 2. Define a function to compute such layouts. (You might find lzw helpful.)

type Layout = [ Layer ]
type Layer = [ Connection ] 
type Connection = (Size, Size)
layout :: Circuit -> Layout


How long does your function take? If it isn’t linear in the size of the term describing the circuit, can you make it so?

> module Q13_prog where

> import Q1_prog
> import Q12_prog
> import Control.Monad.Writer

> type Layout = [Layer]
> type Layer = [Connection] 
> type Connection = Maybe (Size, Size)


> layout :: Circuit -> Layout
> layout cir = case cir of
>     (Above a b) -> (layout a) ++ (layout b)
>     (_) -> (layer cir) : []
>     where
>         w = width cir
>         inc i = i + 1
>         dec j = j - 1
>         layer l = case l of
>             (Beside a b) -> (layer a) ++ (layer b)
>             (_) -> (calc l 0) : [] 
>         calc a i = case a of
>             (Stretch (xs) f) -> Just ((head xs) -1 , sum(tail(xs)) + 1)
>             (Fan f) -> if i <= w then Just (i , inc i) else Just (f, f)
>             (Id i) -> Nothing


Sort of :-s

*Q13_prog> layout ((Fan 2 `Beside` Fan 2) `Above` Stretch [ 2, 2 ] (Fan 2) `Above` (Id 1 `Beside` Fan 2 `Beside` Id 1))
[[Just (0,1),Just (0,1)],[Just (1,3)],[Nothing,Just (0,1),Nothing]]


Starting simple

 layout :: Circuit -> Layout
 layout cir = case cir of
     (Beside a b) -> [(count a), (count b)] : []
     (cir) -> layout cir
     where 
         count a = case a of
             (Id i) -> (i, i)

*Q13_prog> layout ((Id 1) `Beside` (Id 1))
[[(1,1),(1,1)]]


Works to some extent but falls apart apart from this case...


 layout :: Circuit -> Layout
 layout cir = case cir of
     (Above a b) -> [(count a w)] : [(count b w)] : []
     where
         w = width cir 
         count a w = case a of
             (Fan f) -> ((f - w), f - w) + 1)

*Q13_prog> layout ((Fan 2) `Above` (Fan 2))
[[(0,1)],[(0,1)]]



So the circuit

((Fan 2 `Beside` Fan 2) `Above` Stretch [ 2, 2 ] (Fan 2) `Above` (Id 1 `Beside` Fan 2 `Beside` Id 1))

translates to

[[(0, 1), (2, 3)], [(1, 3)], [(1, 2)]] ! However this somewhat contradicts the definition of the stretch combinator that appears to start from 1 as the base value, not 0!! 

Thinking in terms of iterators, pass the circuit, seperating out each layer by 'Above' combinator

((Fan 2 `Beside` Fan 2) `Above` Stretch [ 2, 2 ] (Fan 2) `Above` (Id 1 `Beside` Fan 2 `Beside` Id 1))

Calculate the width of a layer

(Fan 2 `Beside` Fan 2) = 4
Stretch [ 2, 2 ] (Fan 2) = 4
Id 1 `Beside` Fan 2 `Beside` Id 1 = 4

Deal with a simple base case first:

(Fan 2 `Beside` Fan 2) = 4

Take each circuit ether side and increment up to width value, as we did in the Q5 and 6 function

(Fan i w) -> dec w return i
where return
    return r = 



Incrementor style brings the concept of referential transparncy into disrupute, a foundation pilar of FPR. 


