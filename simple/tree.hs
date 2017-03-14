{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Control.Monad.Writer.Lazy

import Data.Maybe

class Syn a where
  syn :: a -> String

class Sem a where
  sem :: a -> String

data Aexpr = Lit Int
           | Var String
           | Add Aexpr Aexpr
           | Times Aexpr Aexpr
           | Difference Aexpr Aexpr

instance Syn Aexpr where
  syn (Lit z) = show z
  syn (Var s) = s
  syn (Add lhs rhs) = (syn lhs) ++ " + " ++ (syn rhs)
  syn (Times lhs rhs) = (syn lhs) ++ " * " ++ (syn rhs)
  syn (Difference lhs rhs) = (syn lhs) ++ " - " ++ (syn rhs)

instance Sem Aexpr where
  sem = syn

data Bexpr = TrueLit
           | FalseLit
           | EqTest Aexpr Aexpr
           | LeqTest Aexpr Aexpr
           | Negation Bexpr
           | Conjunction Bexpr Bexpr

instance Syn Bexpr where
  syn TrueLit = "true"
  syn FalseLit = "false"
  syn (EqTest lhs rhs) = (syn lhs) ++ " = " ++ (syn rhs)
  syn (LeqTest lhs rhs) = (syn lhs) ++ " \\leq " ++ (syn rhs)
  syn (Negation x) = "\\neg (" ++ (syn x) ++ ")"
  syn (Conjunction lhs rhs) = (syn lhs) ++ " \\land " ++ (syn rhs)
  
instance Sem Bexpr where
  sem TrueLit = "\\top"
  sem FalseLit = "\\bot"
  sem (EqTest lhs rhs) = (sem lhs) ++ " = " ++ (sem rhs)
  sem (LeqTest lhs rhs) = (sem lhs) ++ " \\leq " ++ (sem rhs)
  sem (Negation x) = "\\neg (" ++ (sem x) ++ ")"
  sem (Conjunction lhs rhs) = (sem lhs) ++ " \\land " ++ (sem rhs)

data Stmt = Assignment String Aexpr
          | Skip
          | Seq Stmt Stmt
          | IfThenElse Bexpr Stmt Stmt
          | While Bexpr Stmt

instance Syn Stmt where
  syn (Assignment lhs rhs) = lhs ++ " := " ++ (syn rhs)
  syn Skip = "\\text{skip}"
  syn (Seq a b) = (syn a) ++ "; " ++ (syn b)
  syn (IfThenElse cond th els) = "\\text{if} (" ++ (syn cond) ++ ") \\text{then} { " ++ (syn th) ++ " } \\text{else} { " ++ (syn els) ++ " }";
  syn (While cond body) = "\\text{while} (" ++ (syn cond) ++ ") { " ++ (syn body) ++ " }"

type State = [(String, Int)]

replace :: String -> Int -> State -> State
replace lhs rhs σ = (lhs, rhs) : (filter ((lhs /=) . fst) σ)

valof :: String -> State -> Int
valof s σ = fromMaybe 0 (lookup s σ)

instance Sem State where
  sem [] = "\\sigma"
  sem ((var, val) : xs) = (sem xs) ++ "[" ++ var ++ "\\mapsto " ++ (show val) ++ "]"

instance Sem Bool where
  sem True = "\\top"
  sem False = "\\bot"

instance Sem Int where
  sem = show

evalA :: Aexpr -> State -> Writer String Int
evalA term σ = case term of
  (Lit n) -> writer (n, "\\prftree[r]{\\text{[lit\\textsubscript{ns}]}}{}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem n) ++ "}}")
  (Var var) -> let val = valof var σ in writer (val, "\\prftree[r]{\\text{[st\\textsubscript{ns}]}}{}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem val) ++ "}}")
  (Add lhs rhs) -> do
    let (lval, lpf) = runWriter (evalA lhs σ)
    let (rval, rpf) = runWriter (evalA rhs σ)
    let normal = lval + rval
    writer (normal, "\\prftree[r]{\\text{[sum\\textsubscript{ns}]}}{" ++ lpf ++ "}{" ++ rpf ++ "}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem normal) ++ "}}")
  (Times lhs rhs) -> do
    let (lval, lpf) = runWriter (evalA lhs σ)
    let (rval, rpf) = runWriter (evalA rhs σ)
    let normal = lval * rval
    writer (normal, "\\prftree[r]{\\text{[mul\\textsubscript{ns}]}}{" ++ lpf ++ "}{" ++ rpf ++ "}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem normal) ++ "}}")
  (Difference lhs rhs) -> do
    let (lval, lpf) = runWriter (evalA lhs σ)
    let (rval, rpf) = runWriter (evalA rhs σ)
    let normal = lval - rval
    writer (normal, "\\prftree[r]{\\text{[sub\\textsubscript{ns}]}}{" ++ lpf ++ "}{" ++ rpf ++ "}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem normal) ++ "}}")

evalB :: Bexpr -> State -> Writer String Bool
evalB term σ = case term of
  TrueLit -> writer (True, "\\prftree[r]{[tt\\textsubscript{ns}]}{}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem term) ++ "}}")
  FalseLit -> writer (False, "\\prftree[r]{\\text{[ff\\textsubscript{ns}]}}{}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem term) ++ "}}")
  (EqTest a b) -> do
    let (lhs, lpf) = runWriter (evalA a σ)
    let (rhs, rpf) = runWriter (evalA b σ)
    let normal = lhs == rhs
    writer (normal, "\\prftree[r]{\\text{[eq\\textsubscript{ns}]}}{" ++ lpf ++ "}{" ++ rpf ++ "}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem normal) ++ "}}")
  (LeqTest a b) -> do
    let (lhs, lpf) = runWriter (evalA a σ)
    let (rhs, rpf) = runWriter (evalA b σ)
    let normal = lhs <= rhs
    writer (normal, "\\prftree[r]{\\text{[leq\\textsubscript{ns}]}}{" ++ lpf ++ "}{" ++ rpf ++ "}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem normal) ++ "}}")
  (Negation x) -> do
    let (xval, pf) = runWriter (evalB x σ)
    let normal = not xval
    writer (normal, "\\prftree[r]{\\text{[neg\\textsubscript{ns}]}}{" ++ pf ++ "}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem normal) ++ "}}")
  (Conjunction a b) -> do
    let (lhs, lpf) = runWriter (evalB a σ)
    case lhs of
      False -> do
        writer (lhs, "\\prftree[r]{\\text{[and\\textsubscript{ns}\\textsuperscript{\\LEFTcircle}]}}{" ++ lpf ++ "}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem lhs) ++ "}}")
      True -> do
        let (rhs, rpf) = runWriter (evalB b σ)
        let normal = lhs == rhs
        writer (normal, "\\prftree[r]{\\text{[and\\textsubscript{ns}\\textsuperscript{\\CIRCLE}]}}{" ++ lpf ++ "}{" ++ rpf ++ "}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem normal) ++ "}}")

evalS :: Stmt -> State -> Writer String State
evalS term σ = case term of
  (Assignment lhs rhs) -> do
    let (val, pf) = runWriter (evalA rhs σ)
    let normal = replace lhs val σ
    writer (normal, "\\prftree[r]{\\text{[ass\\textsubscript{ns}]}}{" ++ pf ++ "}{\\rulen{" ++ (syn term) ++"}{" ++ (sem σ) ++ "}{" ++ (sem normal) ++ "}}")
  Skip -> writer (σ, "\\prftree[r]{\\text{[skip\\textsubscript{ns}]}}{\\rulen{" ++ (syn term) ++"}{" ++ (sem σ) ++ "}{" ++ (sem σ) ++ "}}")
  (Seq a b) -> do
    let (bef, bpf) = runWriter (evalS a σ)
    let (aft, apf) = runWriter (evalS b bef)
    writer (aft, "\\prftree[r]{\\text{[comp\\textsubscript{ns}]}}{" ++ bpf ++ "}{" ++ apf ++ "}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem aft) ++ "}}")
  (IfThenElse cond thenS elseS) -> do
    let (condval, condpf) = runWriter (evalB cond σ)
    case condval of
      True -> do
        let (normal, pf) = runWriter (evalS thenS σ)
        writer (normal, "\\prftree[r]{\\text{[if\\textsubscript{ns}\\textsuperscript{tt}]}}{" ++ condpf ++ "}{" ++ pf ++ "}{\\rulen{" ++ (syn term) ++"}{" ++ (sem σ) ++ "}{" ++ (sem normal) ++ "}}")
      False -> do
        let (normal, pf) = runWriter (evalS elseS σ)
        writer (normal, "\\prftree[r]{\\text{[if\\textsubscript{ns}\\textsuperscript{ff}]}}{" ++ condpf ++ "}{" ++ pf ++ "}{\\rulen{" ++ (syn term) ++"}{" ++ (sem σ) ++ "}{" ++ (sem normal) ++ "}}")
  loop@(While cond body) -> do
    let (condval, condpf) = runWriter (evalB cond σ)
    case condval of
      True -> do
        let (after_body, body_pf) = runWriter (evalS body σ)
        let (after_next, next_pf) = runWriter (evalS term after_body)
        writer (after_next, "\\prftree[r]{\\text{[while\\textsubscript{ns}\\textsuperscript{tt}]}}{" ++ condpf ++ "}{" ++ body_pf ++ "}{" ++ next_pf ++ "}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem after_next) ++ "}}")
      False -> do
        writer (σ, "\\prftree[r]{\\text{[while\\textsubscript{ns}\\textsuperscript{ff}]}}{" ++ condpf ++ "}{\\rulen{" ++ (syn term) ++ "}{" ++ (sem σ) ++ "}{" ++ (sem σ) ++ "}}")

factorial :: Stmt
factorial =
  (
    (Assignment "y" (Lit 1)) `Seq`
    (While (Negation ((Var "x") `EqTest` (Lit 1))) (
       (Assignment "y" (Times (Var "y") (Var "x"))) `Seq`
       (Assignment "x" (Difference (Var "x") (Lit 1)))
    ))
  )

lazy :: Stmt
lazy =
  (
    (Assignment "x" (Lit 5)) `Seq`
    (Assignment "y" (Lit 2)) `Seq`
    (IfThenElse (Conjunction (LeqTest (Var "x") (Var "y")) (EqTest (Var "x") (Lit 5)))
     (Assignment "z" (Lit 100))
     (Assignment "z" (Lit (-50)))
    )
  )

full :: Stmt
full =
  (
    (Assignment "x" (Lit 5)) `Seq`
    (Assignment "y" (Lit 2)) `Seq`
    (IfThenElse (Conjunction (LeqTest (Var "y") (Var "x")) (EqTest (Var "x") (Lit 5)))
     (Assignment "z" (Lit 100))
     (Assignment "z" (Lit (-50)))
    )
  )
