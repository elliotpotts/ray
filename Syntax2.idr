-- http://users.eecs.northwestern.edu/~clk800/rand-test-study/_gpwfpfmrd/gpwfpfmrd-2009-10-8-12-02-00.pdf
data ASTTag = ExprT | DeclT | VarT

data AST : (ASTTag -> Type) -> ASTTag -> Type where
  ConstF : Int       ->           AST r ExprT
  AddF   : r ExprT -> r ExprT -> AST r ExprT
  MulF   : r ExprT -> r ExprT -> AST r ExprT
  EVarF  : r VarT  ->           AST r VarT
  LetF   : r DeclT -> r ExprT -> AST r ExprT
  BindF  : r VarT  -> r ExprT -> AST r DeclT
  SeqF   : r DeclT -> r DeclT -> AST r DeclT
  VF     : String  ->           AST r VarT

interface HFunctor (pf : (phi -> Type) -> phi -> Type) where
  hmap : ({aix : phi} -> r aix -> r' aix) -> {id : phi} -> (ix ** pf r ix) -> pf r' ix

implementation HFunctor AST where    
  hmap f (ExprT ** ConstF x)  = ConstF x
  hmap f (ExprT ** AddF x y)  = AddF (f x) (f y)
  hmap f (ExprT ** MulF x y)  = MulF (f x) (f y)
  hmap f (ExprT ** LetF x y)  = LetF (f x) (f y)
  hmap f (DeclT ** BindF x y) = BindF (f x) (f y)
  hmap f (DeclT ** SeqF x y)  = SeqF (f x) (f y)
  hmap f (VarT ** EVarF x)    = EVarF (f x)
  hmap f (VarT ** VF x)       = VF x

data Hfix : (pf : (phi -> Type) -> phi -> Type) -> (phi -> Type) where
  Hin : {pf : (phi -> Type) -> phi -> Type} -> pf (Hfix pf) ix -> Hfix pf ix

Expr' : Type 
Expr' = Hfix AST ExprT

ConstF' : Int -> Expr'
ConstF' x = Hin (ConstF x)

AddF' : Expr' -> Expr' -> Expr'
AddF' x y = Hin (AddF x y)
