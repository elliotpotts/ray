-- http://users.eecs.northwestern.edu/~clk800/rand-test-study/_gpwfpfmrd/gpwfpfmrd-2009-10-8-12-02-00.pdf
data ASTTag = ExprT | DeclT | VarT

data Inhabited : (T : Type) -> T -> Type where
  Taut : {T : Type} -> (v : T) -> Inhabited T v

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

interface HFunctor (Phi : phi -> Type) (pf : (phi -> Type) -> phi -> Type) where
  hmap : ({ix : phi} -> Phi ix -> r ix -> r' ix) -> Phi ix -> pf r ix -> pf r' ix

implementation HFunctor (Inhabited ASTTag) AST where
    hmap f (Taut ExprT)  (ConstF x) = ConstF x
    hmap f (Taut ExprT)  (AddF x y) = AddF  (f (Taut ExprT) x) (f (Taut ExprT) y)
    hmap f (Taut ExprT)  (MulF x y) = MulF  (f (Taut ExprT) x) (f (Taut ExprT) y)
    hmap f (Taut ExprT)  (LetF x y) = LetF  (f (Taut DeclT) x) (f (Taut ExprT) y)
    hmap f (Taut DeclT) (BindF x y) = BindF (f (Taut VarT ) x) (f (Taut ExprT) y)
    hmap f (Taut DeclT)  (SeqF x y) = SeqF  (f (Taut DeclT) x) (f (Taut DeclT) y)
    hmap f (Taut VarT)    (EVarF x) = EVarF (f (Taut VarT ) x)
    hmap f (Taut VarT)       (VF x) = VF x

data Hfix : (phi : Type) -> (pf : (phi -> Type) -> phi -> Type) -> tag -> Type where
  Hin : {phi : Type} -> pf (Hfix phi pf) tag -> Hfix phi pf tag

Expr' : Type 
Expr' = Hfix ASTTag AST ExprT

ConstF' : Int -> Expr'
ConstF' x = Hin (ConstF x)
