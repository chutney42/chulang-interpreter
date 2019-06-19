type Maybe a = Just a | Nothing ;

let x = Just (Just (Just Nothing)) ;

match x with
      	Nothing -> 1
      | Just Nothing -> 2
      | Just (Just (Just (Just Nothing))) -> 3
      | Just (Just (Just Nothing)) -> 4
      | Just (Just Nothing) -> 5
      | _ -> 6 ;