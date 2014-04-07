Task 9 - The next function takes a list ws of nat numbers and a list xs of elements such that the sum ws = length xs and partitions xs into segments accourding to length ws eg
group [ 3, 2, 3, 2 ] "functional" = [ "fun", "ct", "ion", "al" ]


Can you identify two equations that completely define group?
group :: [Size] -> [a] -> [[a]]


It is possible to define group with just two equations. The initial function design catered for empty lists with two differing equations, however this can be optimised to have the same function by rearranging the pattern matching. By assuming that we are indeed dealing with two properly defined lists with more than one element then we can process them as we desire by taking the head value of natural numbers list and then grouping and dropping this amount from the string list. This call is made recursively until either list is exhausted in which case we then match on the following wildcard pattern, returning an empty list and thus completing the function call.


> group :: [Int] -> [a] -> [[a]]
> group (x:xs) ys = (take x ys) : (group xs (drop x ys))
> group _ _ = []

*Main> group [ 3, 2, 3, 2 ] "functional" 
["fun","ct","ion","al"]

*Main> group [] "Testing"
[]

*Main> group [1, 2, 3] ""
["","",""]

*Main> group [ 3, 2, 3, 2, 12, 2, 1, 9, 12, 101, 2, 2] "In computer science, functional programming is a programming paradigm, a style of building the structure and elements of computer programs, that treats computation as the evaluation of mathematical functions and avoids state and mutable data." 
["In ","co","mpu","te","r science, f","un","c","tional pr","ogramming is"," a programming paradigm, a style of building the structure and elements of computer programs, that tr","ea","ts"]
