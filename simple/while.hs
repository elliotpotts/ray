data Aexpr = Lit Int
           | Var String
           | Sum Aexpr Aexpr
           | Product Aexpr Aexpr
           | Difference Aexpr Aexpr

data Bexpr = TrueLit
           | FalseLit
           | EqTest Aexpr Aexpr
           | LeqTest Aexpr Aexpr
           | Negation Bexpr
           | Conjunction Bexpr Bexpr

data Stmt = Assignment String Aexpr
          | Skip
          | Seq Stmt Stmt
          | IfThenElse Bexpr Stmt Stmt
          | While Bexpr Stmt

type State = String -> Int

emptyState :: State
emptyState _ = 0

valof :: String -> State -> Int
valof v σ = σ v

replace :: String -> Int -> State -> State
replace lhs rhs σ = \name -> case name == lhs of
                              True -> rhs
                              False -> σ name

evalA :: Aexpr -> State -> Int
evalA x σ = case x of
  (Lit n) -> n
  (Var v) -> valof v σ
  (Sum lhs rhs) -> (evalA lhs σ) + (evalA rhs σ)
  (Product lhs rhs) -> (evalA lhs σ) * (evalA rhs σ)
  (Difference lhs rhs) -> (evalA lhs σ) - (evalA rhs σ)

evalB :: Bexpr -> State -> Bool
evalB x σ = case x of
  TrueLit -> True
  FalseLit -> False
  (EqTest a b) -> (evalA a σ) == (evalA b σ)
  (LeqTest a b) -> (evalA a σ) <= (evalA b σ)
  (Negation x) -> not (evalB x σ)
  (Conjunction a b) -> (evalB a σ) && (evalB b σ)

evalS :: Stmt -> State -> State
evalS s σ = case s of
  (Assignment lhs rhs) -> replace lhs (evalA rhs σ) σ
  Skip -> σ
  (Seq a b) -> evalS b σ' where σ' = evalS a σ
  (IfThenElse b thenS elseS) -> if evalB b σ then evalS thenS σ else evalS elseS σ
  loop@(While cond body) -> if (evalB cond σ) then (let σ' = evalS body σ in evalS loop σ') else σ

factorial :: Stmt
factorial =
  (
    (Assignment "y" (Lit 1)) `Seq`
    (While (Negation ((Var "x") `EqTest` (Lit 1))) (
       (Assignment "y" (Product (Var "y") (Var "x"))) `Seq`
       (Assignment "x" (Difference (Var "x") (Lit 1)))
    ))
  )

calcFac :: Int -> Int
calcFac x = valof "y" σ where σ = evalS factorial (replace "x" x emptyState)
