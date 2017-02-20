public export data Fix : (Type -> Type) -> Type where
  In : f (Fix f) -> Fix f
