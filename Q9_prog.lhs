Task 9 - The next function takes a list ws of nat numbers and a list xs of elements such that the sum ws = length xs and partitions xs into segments accourding to length ws eg
group [ 3, 2, 3, 2 ] "functional" = [ "fun", "ct", "ion", "al" ]

> group :: [Int] -> [a] -> [[a]]
> group [] _ = []
> group _ [] = []
> group (x:xs) ys = (take x ys) : (group xs (drop x ys))

*Main> group [1,3,5] "This is a string"
["T","his"," is a"]