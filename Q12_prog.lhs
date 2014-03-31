12. Define a ‘long zip with’ function, which is like the standard Haskell function zipWith except that it returns a result as long as its longer argument. If one argument is shorter than the other, the last elements of the result are copied from the corresponding elements of the longer list. The binary operator therefore has to take arguments of a common type, and return a result of that type.

lzw :: (a -> a -> a) -> [a] -> [a] -> [a] 

For example,

lzw (+) [1,2,3,4] [5,6,7] = [6,8,10,4]

> module Q12_prog where

> lzw :: (a -> a -> a) -> [a] -> [a] -> [a]  
> lzw _ [] a = a
> lzw _ a [] = a
> lzw f (x:xs) (y:ys) = f x y : lzw f xs ys


*Main> lzw (+) [1,2,3] [2,4,6,8]
[3,6,9,8]
*Main> lzw (+) [1,2,3] [2,4,6,8,10]
[3,6,9,8,10]
*Main> lzw (*) [1,2,3] [2,4,6,8,10]
[2,8,18,8,10]
*Main> lzw (*) [1,2,3] [2,4,6,8,10,100]
[2,8,18,8,10,100]
*Main> lzw (max) [1,2,3] [2,4,6,8,10,100]
[2,4,6,8,10,100]