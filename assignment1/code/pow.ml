let rec pow ~base ~exp =
    if exp = 0
    then 1
    else base * pow ~base:base ~exp:(exp-1)


let rec pow2 ~base ~exp =
    if exp = 0
    then 1
    else base * pow2 ~exp:(exp-1) ~base:base 

