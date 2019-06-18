let fib x = if x == 0 then 0 else if x == 1 then 1 else fib (x - 1) + fib (x - 2) ;
fib 1 ;
fib 5 ;
fib 10 ;