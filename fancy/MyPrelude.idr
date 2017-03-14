module MyPrelude

%access public export

mapFst : (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

orElse : Maybe a -> a -> Maybe a
orElse Nothing x = Just x
orElse x _ = x
