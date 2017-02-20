module Hfix

%access public export

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

--atach : HFunctor pf => Algebra pf r -> Hfix pf ix -> r ix
--atach alg x = hcata step x id where
--                step : Algebra pf r
--                step lhs = ?step_rhs
                

--foldl' : (b -> a -> b) -> b -> List a -> b
--foldl' f v xs = foldr (λx g => (λa => g (f a x))) id xs v
