import Hfix

%access public export

data NodeTag = AExprT | BExprT | StmtT

data AST : PF NodeTag where
  ALiteral : Int -> AST r AExprT
  AIdent : String -> AST r AExprT
  Sum : (augend : r AExprT) -> (addend : r AExprT) -> AST r AExprT
  Factor : (multiplier : r AExprT) -> (multiplicand : r AExprT) -> AST r AExprT
  Difference : (minuend : r AExprT) -> (subtrahend : r AExprT) -> AST r AExprT
  Quotient : (dividend : r AExprT) -> (divisor : r AExprT) -> AST r AExprT
  BTrue : AST r BExprT
  BFalse : AST r BExprT
  EqTest : r AExprT -> r AExprT -> AST r BExprT
  LTETest : r AExprT -> r AExprT -> AST r BExprT
  Negation : r BExprT -> AST r BExprT
  Conjunction : r BExprT -> r BExprT -> AST r BExprT
  Assign : String -> r AExprT -> AST r StmtT
  Skip : AST r StmtT
  Then : r StmtT -> r StmtT -> AST r StmtT
  IfThenElse : r BExprT -> r StmtT -> r StmtT -> AST r StmtT
  WhileDo : r BExprT -> r StmtT -> AST r StmtT

AExpr : Type
AExpr = Hfix AST AExprT

ALiteral' : Int -> AExpr
ALiteral' n = Hin (ALiteral n)

AIdent' : String -> AExpr
AIdent' s = Hin (AIdent s)

Sum' : (augend : AExpr) -> (addend : AExpr) -> AExpr
Sum' x y = Hin (Sum x y)

Factor' : (multiplier : AExpr) -> (multiplicand : AExpr) -> AExpr
Factor' x y = Hin (Factor x y)

Difference' : (minuend : AExpr) -> (subtrahend : AExpr) -> AExpr
Difference' x y = Hin (Difference x y)

Quotient' : (dividend : AExpr) -> (divisor : AExpr) -> AExpr
Quotient' x y = Hin (Quotient x y)

BExpr : Type
BExpr = Hfix AST BExprT

BTrue' : BExpr
BTrue' = Hin BTrue

BFalse' : BExpr
BFalse' = Hin BFalse

EqTest' : AExpr -> AExpr -> BExpr
EqTest' x y = Hin (EqTest x y)

LTETest' : AExpr -> AExpr -> BExpr
LTETest' x y = Hin (LTETest x y)

Negation' : BExpr -> BExpr
Negation' x = Hin (Negation x)

Conjunction' : BExpr -> BExpr -> BExpr
Conjunction' x y = Hin (Conjunction x y)

Stmt : Type
Stmt = Hfix AST StmtT

Assign' : String -> AExpr -> Stmt
Assign' v x = Hin (Assign v x)

Skip' : Stmt
Skip' = Hin Skip

Then' : Stmt -> Stmt -> Stmt
Then' x y = Hin (Then x y)

IfThenElse' : BExpr -> Stmt -> Stmt -> Stmt
IfThenElse' c t e = Hin (IfThenElse c t e)

WhileDo' : BExpr -> Stmt -> Stmt
WhileDo' c d = Hin (WhileDo c d)

implementation HFunctor AST where
  hmap f (ALiteral n) = ALiteral n
  hmap f (AIdent s) = AIdent s
  hmap f (Sum x y) = Sum (f x) (f y)
  hmap f (Factor x y) = Factor (f x) (f y)
  hmap f (Difference x y) = Difference (f x) (f y)
  hmap f (Quotient x y) = Quotient (f x) (f y)
  hmap f BTrue = BTrue
  hmap f BFalse = BFalse
  hmap f (EqTest x y) = EqTest (f x) (f y)
  hmap f (LTETest x y) = LTETest (f x) (f y)
  hmap f (Negation x) = Negation (f x)
  hmap f (Conjunction x y) = Conjunction (f x) (f y)
  hmap f (Assign v x) = Assign v (f x)
  hmap f Skip = Skip
  hmap f (Then x y) = Then (f x) (f y)
  hmap f (IfThenElse t x y) = IfThenElse (f t) (f x) (f y)
