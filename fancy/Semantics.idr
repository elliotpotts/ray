import Syntax
import Hfix

State : Type
State = String -> Int

fresh : State
fresh = const 0

cond : (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
cond b f g x = case b x of
                    True => f x
                    False => g x
                    

evaluateResult : NodeTag -> Type
evaluateResult AExprT = Int
evaluateResult BExprT = Bool
evaluateResult StmtT = State

evaluate : State -> Hfix AST t -> evaluateResult t
evaluate {t = AExprT} σ = hcata alg where
  alg : FAlgebra AST Main.evaluateResult
  alg (ALiteral n) = n
  alg (AIdent v) = σ v
  alg (Sum x y) = x + y
  alg (Factor x y) = x * y
  alg (Difference x y) = x - y
  alg (Quotient x y) = x `div` y
  
evaluate {t = BExprT} σ = hcata alg where
  alg : FAlgebra AST Main.evaluateResult
  alg BTrue = True
  alg BFalse = False
  alg (EqTest x y) = x == y
  alg (LTETest x y) = x <= y
  alg (Negation x) = not x
  alg (Conjunction x y) = x && y

evaluate {t = StmtT} σ = hcata alg where
  alg : FAlgebra AST Main.evaluateResult
  alg (Assign v x) = \s => case s == v of
                               True => x
                               False => σ s
  alg Skip = σ
  alg (Then x y) = y
  alg (IfThenElse cond cons alt) = if cond then cons else alt
  alg (WhileDo b S) = ?help --fix f where
--    f g = cond b (g . S) id
--    f g = if b then (g . S) else id

--evaluate' : State -> Hfix AST t -> Hfix AST t
--evaluate' {t = AExprT} σ = hana coalg where
--  coalg : Cofalgebra AST Main.evaluateResult
