

in_range min max x = 
    let lower = min <= x
        upper = max >= x
    in 
    lower && upper


in_range' min max x = lower && upper
    where 
        lower = min <= x
        upper = max >= x