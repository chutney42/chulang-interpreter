type Ala a b c = Ala a b c;

let x = Ala True 5 (Ala 4 2 False);

match x with
      Ala False _ _ -> 1 |
      Ala True _ (Ala 4 3 False) -> 2 |
      Ala False 3 (Ala 4 2 True) -> 3 |
      Ala True 2 _ -> 4 ;