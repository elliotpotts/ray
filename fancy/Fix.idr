public export data Fix : (Type -> Type) -> Type where
  In : f (Fix f) -> Fix f

out : Fix f -> f (Fix f)
out (In x) = x

Algebra : (Type -> Type) -> Type -> Type
Algebra f a = f a -> a

Coalgebra : (Type -> Type) -> Type -> Type
Coalgebra f a = a -> f a

cata : Functor f => Algebra f a -> Fix f -> a
cata alg = alg . map (cata alg) . out

ana : Functor f => Coalgebra f a -> a -> Fix f
ana coalg = In . map (ana coalg) . coalg

hylo : Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coalg = (cata alg) . (ana coalg)



-----
data ListF : Type -> Type -> Type where
  Nilf : ListF a r
  Cons : a -> r -> ListF a r

implementation Functor (ListF a) where
  map f Nilf = Nilf
  map f (Cons x y) = Cons x (f y)

FListF : Type -> Type
FListF a = Fix (ListF a)

data BTree : Type -> Type -> Type where
  Leaf : a -> BTree a r
  Node : a -> r -> r -> BTree a r

implementation Functor (BTree a) where
  map f (Leaf x) = Leaf x
  map f (Node x y z) = Node x (f y) (f z)

Btree : Type -> Type
Btree a = Fix (BTree a)

-- Wiki:
--      2
--    /  \
--   7   5
--  / \
-- 2   6
--

wiki : Btree Int
wiki = In $ Node 2 (In $ Node 7 (In $ Leaf 2) (In $ Leaf 6)) (In $ Leaf 5)

sum : Btree Int -> Int
sum = cata alg where
  alg : BTree Int Int -> Int
  alg (Leaf x) = x
  alg (Node x y z) = x + y + z

--depth : Btree Int -> Btree Int
--depth = hylo alg coalg where
--  alg : Algebra Btree Int
--  coalg : Algebra Btree Int
 
