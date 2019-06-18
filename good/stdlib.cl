type Maybe a = Just a | Nothing ;
type Either a b = Left a | Right b ;
type List a = L a (List a) | EOL ;
type Pair a b = P a b ;

let map f l = match l with EOL -> EOL | L h t -> L (f h) (map f t) ;
let filter f l = match l with EOL -> EOL | L h t -> if f h then L h (filter f t) else filter f t ;
let foldr f a l = match l with EOL -> a | L h t -> f h (foldr f a t) ;
let foldl f a l = match l with EOL -> a | L h t -> foldl f (f a h) t ;
let append l r = foldr L r l ;

let fst p = match p with P l _ -> l ;
let snd p = match p with P _ r -> r ;
