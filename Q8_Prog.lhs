8. We say that the type s forms a semigroup if it supports an associative binary
operator 

class Semigroup s where
(⊕)::s ➝ s ➝ s -- associative

This is like the more familiar type class Monoid, except without the unit element. For example, integers form a semigroup using addition:

instance Semigroup Int where
   (⊕) = (+)

 Here is another semigroup, of ‘kissing segments’. Represent a segment of the natural number line by a pair of integers <i,j> with i <= j. Say that one such segment <i, j> ‘kisses’ another <k,l> precisely when k = j + 1; then they combine to yield the segment <i,l>. Show that this kissing combination operation is associative, when it is defined. Define a datatype of segments, and make it an instance of Semigroup.

data Segment = ...
instance Semigroup Segment where ...

> module Q8_Prog where

> import Q1_prog
> -- import Data.Semigroup

> class Semigroup s where
>     exor :: s -> s -> s -- associative
> 

> instance Semigroup Int where
>     exor = (+)

> data Segment = 
>    Segment (Circuit, Circuit)

> data Segment' = 
>    Segment' { x :: Circuit
>             , y :: Circuit
>             }

Seems to be along the right lines...

*Q8_Prog> let y = Segment((Id 1),(Id 1))
*Q8_Prog> :t y
y :: Segment

*Q8_Prog> let y = Segment'{x = (Id 1), y = (Id 1)}
*Q8_Prog> :t y
y :: Segment'

On second thoughts, the example above is somewhat vague, stick to handling Ints for now and not circuits...

> data SegmentInt = 
>    SegmentInt { i :: Int
>               , j :: Int
>               }
>               deriving(Show, Eq)

*Q8_Prog> let s = SegmentInt 12 23
*Q8_Prog> :t s
s :: SegmentInt
*Q8_Prog> s
SegmentInt {i = 12, j = 23}


Now to make it an instance of semigroup...
