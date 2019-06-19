type Ala a b c = Ala a b c ;

let id x = x ;

id True ;
id 42 ;
id (Ala 1 2 3);
id (Ala (Ala True 4 2) True 0);