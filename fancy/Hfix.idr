module Hfix

%access public export

fix : (a -> a) -> a
fix f = f (fix f)

-- http://users.eecs.northwestern.edu/~clk800/rand-test-study/_gpwfpfmrd/gpwfpfmrd-2009-10-8-12-02-00.pdf
PF : Type -> Type
PF phi = (r : phi -> Type) -> (ix : phi) -> Type

interface HFunctor (pf : PF phi) where
  hmap : ({ix : phi} -> r ix -> r' ix) -> pf r ix -> pf r' ix

data Hfix : PF phi -> phi -> Type where
  Hin : {pf : PF phi} -> pf (Hfix pf) ix -> Hfix pf ix

data Hfree : PF phi -> phi -> Type where
  Hfin : {pf : PF phi} -> pf (Hfree pf) ix -> Hfree pf ix
  Hpure : {pf : PF phi} -> (ix : phi) -> Hfree pf ix

--implementation HFunctor f => HFunctor (Hfree f) where
--  hmap f (Hpure x) = Hpure (f x)
--  hmap f (Hfin x) = Hfin (map (map f) x)

Hout : Hfix pf r -> pf (Hfix pf) r
Hout (Hin x) = x

FAlgebra : (pf : PF phi) -> (phi -> Type) -> Type
FAlgebra pf {phi} r = {ix : phi} -> pf r ix -> r ix

hcata : HFunctor pf => FAlgebra pf r -> Hfix pf ix -> r ix
hcata alg = alg . hmap (hcata alg) . Hout

Cofalgebra : (pf : PF phi) -> (phi -> Type) -> Type
Cofalgebra pf {phi} r = {ix : phi} -> r ix -> pf r ix

hana : HFunctor pf => Cofalgebra pf r -> r ix -> Hfix pf ix
hana coalg = Hin . hmap (hana coalg) . coalg

RAlgebra : HFunctor pf => (pf : PF phi) -> (phi -> Type) -> Type
--RAlgebra pf {phi} r = FAlge
--RAlgebra pf {phi} r = {ix : phi} -> pf (Int, r ix) -> r ix

-- type RAlgebra pf r = pf (Term pf, r) -> r



--atach : HFunctor pf => Algebra pf r -> Hfix pf ix -> r ix
--atach alg x = hcata step x id where
--                step : Algebra pf r
--                step lhs = ?step_rhs
                

--foldl' : (b -> a -> b) -> b -> List a -> b
--foldl' f v xs = foldr (λx g => (λa => g (f a x))) id xs v
