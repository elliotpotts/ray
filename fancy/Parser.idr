import MyPrelude

%access public export

ParseResult : Type -> Type
ParseResult a = List (a, String)

data Parser : Type -> Type where
  MkParser : (p : String -> ParseResult a) -> Parser a

parse : String -> Parser a -> ParseResult a
parse s (MkParser p) = p s

implementation Functor Parser where
    map f (MkParser p) = MkParser (\s => map (mapFst f) (p s))

produce : a -> Parser a
produce x = MkParser (\s => [(x, s)])

implementation Applicative Parser where
    pure = produce
    (<*>) x y = MkParser parseBs where
                parseA2Bs : String -> ParseResult (a -> b)
                parseA2Bs s = parse s x
                ap : ParseResult (a -> b) -> ParseResult b
                ap [] = []
                parseBs : String  -> ParseResult b
                ap ((z, rest) :: xs) = ?ap_rhs_2
                parseBs s = ap (parseA2Bs s)

                
                             
failure : Parser a
failure = MkParser (\_ => [])

success : Parser ()
success = produce ()

implementation Alternative Parser where
    empty = failure
    (<|>) (MkParser p) (MkParser q) = MkParser (\s => case p s of
                                                          [] => q s
                                                          (x :: xs) => [x])
---------------------------------
-- Combinators
---------------------------------
many : Parser a -> Parser (List a)
many p = MkParser f
         where f s = case parse s p of
                          [] => []
                          ((x, rest) :: xs) => ?rhs

optional : Parser a -> Parser (Maybe a)
optional p = Just <$> p <|> produce Nothing
---------------------------------
-- Parsers
---------------------------------
lit : String -> Parser String
lit x = MkParser matchLit where
          matchLit s = case isPrefixOf x s of
                            True => [(x, pack (drop (length x) (unpack s)))]
                            False => []

skipWs : Parser ()
skipWs = MkParser (\s => [((), ltrim s)])

tok : String -> Parser String
tok x = skipWs *> lit x

one : (Char -> Bool) -> Parser Char
one f = MkParser (\s => case unpack s of
                         [] => []
                         (x :: xs) => if f x then [(x, pack xs)]
                                    else [])

consecutive : (Char -> Bool) -> Parser String
consecutive p = MkParser doParse
                  where doParse s = (pack taken, pack remain) :: []
                    where unpacked : List Char
                          unpacked = unpack s
                          taken : List Char
                          taken = takeWhile p unpacked
                          remain : List Char
                          remain = drop (length taken) unpacked

digit : Parser Char
digit = one isDigit

alpha : Parser Char
alpha = one isAlpha

alphaNum : Parser Char
alphaNum = one isAlphaNum

word : Parser String
word = MkParser (\s => case words s of
                            [] => []
                            (x :: xs) => [(x, unwords xs)])

