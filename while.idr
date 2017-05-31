import Data.Vect

%default total

Num : Type
Num = Int

Var : Type
Var = String

data Aexp : Type where
  N : Num -> Aexp
  V : Var -> Aexp
  Sum : Aexp -> Aexp -> Aexp
  Product : Aexp -> Aexp -> Aexp
  Difference : Aexp -> Aexp -> Aexp

data Bexp : Type where
  TT : Bexp
  FF : Bexp
  EQ : Aexp -> Aexp -> Bexp
  LEQ : Aexp -> Aexp -> Bexp
  Complement : Bexp -> Bexp
  Conjunction : Bexp -> Bexp -> Bexp
  
data Stm : Type where
  Ass : Var -> Aexp -> Stm
  Skip : Stm
  Sequence : Stm -> Stm -> Stm
  If : Bexp -> Stm -> Stm -> Stm
  While : Bexp -> Stm -> Stm

-- Semantics
St : Type
St = Var -> Num

a : (x : Aexp) -> (st : St) -> Num
a (N x) st = x
a (V x) st = st x
a (Sum x y) st = (a x st) + (a y st)
a (Product x y) st = (a x st) * (a y st)
a (Difference x y) st = (a x st) - (a y st)

b : (x : Bexp) -> (st : St) -> Bool
b TT st = True
b FF st = False
b (EQ x y) st = (a x st) == (a y st)
b (LEQ x y) st = (a x st) >= (a y st)
b (Complement x) st = not (b x st)
b (Conjunction x y) st = (b x st) && (b y st)

partial fix : ((a -> b) -> (a -> b)) -> (a -> b)
fix f = f (fix f)

fixpoint_pf : (x : a) -> (f : a -> b) -> Type
fixpoint_pf x f = x = f x

cond : (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
cond c f g x with (c x)
  | True = f x
  | False = g x

sub : Eq a => (a -> b) -> a -> b -> (a -> b)
sub f x' fx' x with (x == x')
  | True = fx'
  | False = f x

partial s : (x : Stm) -> (st : St) -> St
s (Ass x y) = \st => \v => if x == v then (a y st) else st x
s Skip = id
s (Sequence x y) = \st => let st' = s y st in s x st'
s (If c x y) = \st => if (b c st) then (s x st) else (s y st)
s (While c x) = fix f where
  partial f : (St -> St) -> (St -> St)
  f g = cond (b c) (g . (s x)) (the (St -> St) id)

--- Abstract Machine
Stack : Type
Stack = List (Either Num Bool)

data Inst : Type where
  PushN : Num -> Inst
  Add : Inst
  Mult : Inst
  Sub : Inst
  PushTT : Inst
  PushFF : Inst
  TestEQ : Inst
  TestLE : Inst
  And : Inst
  Neg : Inst
  Fetch : Var -> Inst
  Store : Var -> Inst
  Noop : Inst
  Branch : List Inst -> List Inst -> Inst
  Loop : List Inst -> List Inst -> Inst

St' : Type
St' = List (Var, Num)

data AMConf : Type where
  MkAm : List Inst -> (stk : Stack) -> (st : St') -> AMConf

data Step : Type where
  Nont : AMConf -> Step
  Term : AMConf -> Step
  Stuck : AMConf -> Step 

transition : AMConf -> Step
transition (MkAm [] stk st) = Term (MkAm [] stk st)
transition (MkAm ((PushN n) :: xs) stk st) = Nont $ MkAm xs ((Left n) :: stk) st
transition (MkAm (Add :: xs) ((Left z1)::(Left z2)::stk) st) = Nont $ MkAm xs ((Left $ z1 + z2)::stk) st
transition (MkAm (Mult :: xs) ((Left z1)::(Left z2)::stk) st) = Nont $ MkAm xs ((Left $ z1 * z2)::stk) st
transition (MkAm (Sub :: xs) ((Left z1)::(Left z2)::stk) st) = Nont $ MkAm xs ((Left $ z1 - z2)::stk) st
transition (MkAm (PushTT :: xs) stk st) = Nont $ MkAm xs ((Right True) :: stk) st
transition (MkAm (PushFF :: xs) stk st) = Nont $ MkAm xs ((Right False) :: stk) st
transition (MkAm (TestEQ :: xs) ((Left z1)::(Left z2)::stk) st) = Nont $ MkAm xs ((Right $ z1 == z2)::stk) st
transition (MkAm (TestLE :: xs) ((Left z1)::(Left z2)::stk) st) = Nont $ MkAm xs ((Right $ z1 <= z2)::stk) st
transition (MkAm (And :: xs) ((Right b1)::(Right b2)::stk) st) = Nont $ MkAm xs ((Right $ b1 && b2)::stk) st
transition (MkAm (Neg :: xs) ((Right b)::stk) st) = Nont $ MkAm xs ((Right $ not b)::stk) st
transition am@(MkAm ((Fetch v) :: xs) stk st) = case lookup v st of
                                                 Just val => Nont $ MkAm xs ((Left val)::stk) st
                                                 Right => Stuck am
transition (MkAm ((Store v) :: xs) ((Left z)::stk) st) = Nont $ MkAm xs stk ((v, z) :: st)
transition (MkAm (Noop :: xs) stk st) = Nont $ MkAm xs stk st
transition (MkAm ((Branch ys zs) :: xs) ((Right True)::stk) st) = Nont $ MkAm (ys ++ xs) stk st
transition (MkAm ((Branch ys zs) :: xs) ((Right False)::stk) st) = Nont $ MkAm (zs ++ xs) stk st
transition (MkAm ((Loop ys zs) :: xs) stk st) = Nont $ MkAm (ys ++ (Branch (zs ++ [Loop ys zs]) [Noop]) :: xs) stk st
transition conf = Stuck conf

step : AMConf -> AMConf
step x = case transition x of
           Nont x' => x'
           Term x' => x'
           Stuck x' => x'

--
foo : AMConf
foo = MkAm [PushN 1, Fetch "x", Add, Store "x"] [] [("x", 3)]

data StepK : AMConf -> AMConf -> Nat -> Type where
  StartWith : (start : AMConf) -> StepK start start Z
  StepOnce : StepK start middle k -> StepK start (step middle) (S k)

foo_pf : StepK Main.foo (MkAm [] [] [("x", 4), ("x", 3)]) 4
foo_pf = StepOnce $ StepOnce $ StepOnce $ StepOnce $ StartWith Main.foo

extend : AMConf -> List Inst -> Stack -> AMConf
extend (MkAm xs stk st) xs' stk' = MkAm (xs ++ xs') (stk ++ stk') st

--ext_pf : (start : AMConf) -> (end : AMConf) -> (pf : StepK start end k) ->
--         (xs : List Inst) -> (stk : Stack) -> StepK (extend start xs stk) end k
