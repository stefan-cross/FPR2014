A library of functions specifying and manipulating parallel prefix circuits. Prefix depends on an associative binary operator (+). A prefix computation of size n takes a list input [x1, x2, ..., xn] of lenght n and returns the output of the list [x1, x1 + x2, ..., x1 + x2 + ... xn] - the sums of the none empty prefixes of the input list...

> type Size = Int -- natural numbers
> data Circuit
>     = Id Size
>     | Fan Size
>     | Above Circuit Circuit
>     | Beside Circuit Circuit
>     | Stretch [Size] Circuit


width, depth :: Circuit -> Size

wellsized :: Circuit -> Bool
