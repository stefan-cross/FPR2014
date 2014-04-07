10. Hence define a function to apply a circuit to a list of the appropriate length whose elements are drawn from a semigroup. If the circuit is indeed a parallel prefix circuit (say, defined by scan, or serial, or brentkung), then apply really should compute prefix sums.

apply :: Semigroup a => Circuit -> [a] -> [a]

A somewhat surprising result is that a single testcase serves to prove that a purported parallel prefix circuit is correct. Assuming that the circuit has width w, it should take the list of point segments <1, 1>, <2, 2>, ..., <w, w> to the list of prefixes <1, 1>, <1, 2>, ..., <1, w>, using the semigroup of kissing combination. Conversely, if it works for this test case, then it works for any semigroup! You may find this result helpful in checking that you got your definition of apply (and of brentkung) correct.


> module Q10_prog where
> import Q1_prog
> import Q2_prog
> import Q3_prog
> import Q4_prog
> import Q5_prog
> import Q6_prog
> import Q8_prog

> apply :: Semigroup a => Circuit -> [a] -> [a]
> apply cir xs 
>     | (width cir) == (length xs) = apply' xs
>     | otherwise = error "Circuit and segment list length mismatch"
>     where
>         apply' (x:xs) = x ⊕ (apply' xs)


*Q10_prog> let cir = (Above (Beside (Fan 2) (Id 1)) (Beside (Id 1) (Fan 2)))
*Q10_prog> apply cir [(Segment (1, 1)), (Segment (2, 2)), (Segment (3, 3))]
[(Segment (1, 3)]