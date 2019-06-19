type Ala a b = Ala a b ;

let ala = Ala 3 4 ;

let f x (Ala y z) = x + y + z ; // Syntax error