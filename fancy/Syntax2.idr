-- http://users.eecs.northwestern.edu/~clk800/rand-test-study/_gpwfpfmrd/gpwfpfmrd-2009-10-8-12-02-00.pdf
PF : Type -> Type
PF phi = (r : phi -> Type) -> (ix : phi) -> Type

interface HFunctor (pf : PF phi) where
  hmap : ({ix : phi} -> r ix -> r' ix) -> pf r ix -> pf r' ix

data Hfix : PF phi -> phi -> Type where
  Hin : {pf : PF phi} -> pf (Hfix pf) ix -> Hfix pf ix

Hout : Hfix pf r -> pf (Hfix pf) r
Hout (Hin x) = x

Algebra : PF phi -> (phi -> Type) -> Type
Algebra pf {phi} r = {ix : phi} -> pf r ix -> r ix

hcata : HFunctor pf => Algebra pf r -> Hfix pf ix -> r ix
hcata alg = alg . hmap (hcata alg) . Hout

------------

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

implementation HFunctor AST where
  hmap f (ConstF x)  = ConstF x
  hmap f (AddF x y)  = AddF (f x) (f y)
  hmap f (MulF x y)  = MulF (f x) (f y)
  hmap f (LetF x y)  = LetF (f x) (f y)
  hmap f (BindF x y) = BindF (f x) (f y)
  hmap f (SeqF x y)  = SeqF (f x) (f y)
  hmap f (EVarF x)   = EVarF (f x)
  hmap f (VF x)      = VF x

exalg : Algebra AST (
        \t => case t of
                  ExprT => Int
                  DeclT => (String, Int)
                  VarT => String
        )
exalg (ConstF x) = x
exalg (AddF x y) = x + y
exalg (BindF v x) = (v, x)
exalg (VF v) = v

Expr' : Type 
Expr' = Hfix AST ExprT

Decl' : Type
Decl' = Hfix AST DeclT

Var' : Type
Var' = Hfix AST VarT

ConstF' : Int -> Expr'
ConstF' x = Hin (ConstF x)

AddF' : Expr' -> Expr' -> Expr'
AddF' x y = Hin (AddF x y)

BindF' : Var' -> Expr' -> Decl'
BindF' v x = Hin (BindF v x)

Sum : Expr'
Sum = AddF' (ConstF' 1) (ConstF' 2)

Binding : Decl'
Binding = BindF' (Hin (VF "three")) Sum
