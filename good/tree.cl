// import stdlib.cl (stack exec interpreter good/stdlib.cl good/tree.cl)

type Tree a = Node a (Tree a) (Tree a) | Leaf ;

type NotFound = NotFound ;

let lookup x t = match t with
    Leaf -> Left NotFound |
    Node (P k v) l r ->
    	 if k == x
	 then Right v
	 else match lookup x l with
	      Left _ -> lookup x r |
	      found -> found ;

let tree1 = Node (P 1 23) (Node (P 2 4242) (Node (P 3 24) Leaf Leaf) (Node (P 4 10) (Node (P 5 81) Leaf Leaf) Leaf)) (Node (P 6 66) (Node (P 7 42) Leaf (Node (P 8 44) Leaf Leaf)) Leaf) ;

lookup 7 tree1;
