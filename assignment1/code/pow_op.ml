let rec pow ?(base=2) ~exp =
    if exp = 0
    then 1
    else base * pow ~exp:(exp-1)
