type Ala a b c = Ala a b c ;
let y = Ala 1 2 3 ;
match y with Ala 1 2 3 4 -> 5 ; // Should throw: "Construtor Ala should have 3 arguments, but has been given 4"