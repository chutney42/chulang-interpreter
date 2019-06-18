// import stdlib.cl

let indexOf x l = let f x l c = match l with EOL -> Nothing | L h t -> if x == h then Just c else f x t (c + 1) in f x l 0 ;

let quickSort l = match l with EOL -> EOL | L h t -> let pl = filter (\x. x <= h) t, pr = filter (\x. x > h) t in append (quickSort pl) (L h (quickSort pr)) ;

let list1 = L 0 (L 1 (L 2 (L 3 (L 4 (L 5 (L 6 (L 7 (L 8 (L 9 EOL))))))))) ;
let list2 = L 5 (L 1 (L 3 (L 8 (L 3 (L 2 (L 4 (L 6 (L 1 (L 0 EOL))))))))) ;

indexOf 5 list1 ;
quickSort list2 ;