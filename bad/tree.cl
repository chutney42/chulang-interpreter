type Tree a = Leaf | Node a (Tree a) (Tree a);
let t = Node 5 (Node 6 (Leaf) (Node 7 (Node 8 Leaf Leaf) Leaf)) Leaf;
t;