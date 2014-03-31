Task 9 - The next function takes a list ws of nat numbers and a list xs of elements such that the sum ws = length xs and partitions xs into segments accourding to length ws eg
group [ 3, 2, 3, 2 ] "functional" = [ "fun", "ct", "ion", "al" ]

> group :: [Int] -> [a] -> [[a]]
> group [] _ = []
> group _ [] = []
> group (x:xs) ys = (take x ys) : (group xs (drop x ys))

*Main> group [1,3,5] "This is a string"
["T","his"," is a"]



Can you identify two equations that completely define group?
group :: [Size] -> [a] -> [[a]]

We can have three equations that can completely define the function group which cover all the exhaustive patterns. We must cater for the possibility that neither an empty list is passed in for the Int list and also the String list. We are then able to handle workings of the function safe in the knowledge that we are dealing with lists which contain sufficient elements not to produce an error with our list operations. Using the Peno design pattern and catering for the base cases we can then assume that in the third equation we are indeed dealing with non-empty list and the operations take and drop will not produce errors.

Actually, yes we can by reordering and using two wild cards:

> group' :: [Int] -> [a] -> [[a]]
> group' (x:xs) ys = (take x ys) : (group xs (drop x ys))
> group' _ _ = []