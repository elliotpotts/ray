module Polyvariadic

%access public export


take4 : Int -> Int -> Int -> Int -> Int
take4 a b c d = a + b + c + d


interface OutsideType (i : Type) where
  outside : (i -> b) -> (a -> i) -> (a -> b)

implementation OutsideType (a -> b) where
  outside f g = ?OutsideType_rhs_1

implementation OutsideType t where
