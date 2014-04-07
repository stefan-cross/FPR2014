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

> type Layout' = [Layer']
> type Layer' = [Connection'] 
> type Connection' = Maybe (Size, Size)

> layout' :: Circuit -> Layout'
> layout' cir = case cir of
>     (Above a b) -> (layout' a) ++ (layout' b)
>     (_) -> (layer cir) : []
>     where
>         w = width cir
>         layer l = case l of
>             (Beside a b) -> (layer a) ++ (layer b)
>             (_) -> (calc l 0) : [] 
>         calc a i = case a of
>             (Stretch (xs) f) -> Just ((head xs) -1, (sum xs)-1)
>             (Fan f) -> Just (i, (i + f - 1))
>             (Id i) -> Nothing



*Q13_prog> layout ((Fan 2 `Beside` Fan 2) `Above` Stretch [ 2, 2 ] (Fan 2) `Above` (Id 1 `Beside` Fan 2 `Beside` Id 1))
[[Just (0,1),Just (0,1)],[Just (1,3)],[Nothing,Just (0,1),Nothing]]


Incrementor style brings the concept of referential transparncy into disrupute, a foundation pilar of FPR. 


