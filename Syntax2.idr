-- http://users.eecs.northwestern.edu/~clk800/rand-test-study/_gpwfpfmrd/gpwfpfmrd-2009-10-8-12-02-00.pdf
data ASTTag = ExprT | DeclT | VarT

data Inhabited : (T : Type) -> T -> Type where
  Eg : {T : Type} -> (v : T) -> Inhabited T v

data AST : (ASTTag -> Type) -> ASTTag -> Type where
  ConstF : Int       ->           AST r ExprT
  AddF   : r ExprT -> r ExprT -> AST r ExprT
  MulF   : r ExprT -> r ExprT -> AST r ExprT
  EVarF  : r VarT  ->           AST r VarT
  LetF   : r DeclT -> r ExprT -> AST r ExprT
  BindF  : r VarT  -> r ExprT -> AST r DeclT
  SeqF   : r DeclT -> r DeclT -> AST r DeclT
  VF     : String  ->           AST r VarT

--  ix : phi         = ExprT, DeclT, etc.
-- phi : Type        = ASTTag
-- Phi : phi -> Type = ASTType

interface HFunctor (pf : (phi -> Type) -> phi -> Type) where
  hmap : ({ix : phi} -> (Inhabited phi) ix -> r ix -> r' ix) -> (Inhabited phi) ix -> pf r ix -> pf r' ix

implementation HFunctor AST where
    hmap f (Eg ExprT)  (ConstF x) = ConstF x
    hmap f (Eg ExprT)  (AddF x y) = AddF  (f (Eg ExprT) x) (f (Eg ExprT) y)
    hmap f (Eg ExprT)  (MulF x y) = MulF  (f (Eg ExprT) x) (f (Eg ExprT) y)
    hmap f (Eg ExprT)  (LetF x y) = LetF  (f (Eg DeclT) x) (f (Eg ExprT) y)
    hmap f (Eg DeclT) (BindF x y) = BindF (f (Eg VarT ) x) (f (Eg ExprT) y)
    hmap f (Eg DeclT)  (SeqF x y) = SeqF  (f (Eg DeclT) x) (f (Eg DeclT) y)
    hmap f (Eg VarT)    (EVarF x) = EVarF (f (Eg VarT ) x)
    hmap f (Eg VarT)       (VF x) = VF x

data Hfix : (phi : Type) -> (pf : (phi -> Type) -> phi -> Type) -> tag -> Type where
  Hin : {phi : Type} -> pf (Hfix phi pf) tag -> Hfix phi pf tag

Expr' : Type 
Expr' = Hfix ASTTag AST ExprT

ConstF' : Int -> Expr'
ConstF' x = Hin (ConstF x)
