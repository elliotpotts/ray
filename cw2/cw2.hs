import Prelude hiding (product, sum, Num)
import Control.Monad
import Text.Megaparsec hiding (parse, State)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Lexer as Lexer
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Data.List hiding (product, sum)

--------------------------------------------------
-- Syntax
--------------------------------------------------

type Num = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]

data Aexp = N Num | V Var | Mult Aexp Aexp | Add Aexp Aexp | Sub Aexp Aexp
data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp | Le Aexp Aexp | Eq Aexp Aexp
data Stm = Skip | Ass Var Aexp | Comp Stm Stm | If Bexp Stm Stm | While Bexp Stm | Block DecV DecP Stm | Call Pname

----------------------------------------
-- Boilerplate
----------------------------------------

instance Show Aexp where
  show (N n) = show n
  show (V var) = var
  show (Mult a b) = "(" ++ (show a) ++ " * " ++ (show b) ++ ")"
  show (Add a b) = "(" ++ (show a) ++ " + " ++ (show b) ++ ")"
  show (Sub a b) = "(" ++ (show a) ++ " - " ++ (show b) ++ ")"

instance Show Bexp where
  show TRUE = "true"
  show FALSE = "false"
  show (Neg x) = "!(" ++ (show x) ++ ")"
  show (And a b) = "(" ++ (show a) ++ " & " ++ (show b) ++ ")"
  show (Le a b) = (show a) ++ " <= " ++ (show b)
  show (Eq a b) = (show a) ++ " = " ++ (show b)

indent :: Int -> String
indent n = concat $ replicate (n * 2) " "

showStm :: Int -> Stm -> String
showStm n Skip = (indent n) ++ "skip"
showStm n (Ass var val) = (indent n) ++ var ++ " := " ++ (show val)
showStm n (Comp a b) = (showStm n a) ++ ";\n" ++ (showStm n b)
showStm n (If cond thn els) = (indent n) ++ "if " ++ (show cond) ++ " then\n" ++ (showStm (n+1) thn) ++ "\n" ++ (indent n) ++ "else\n" ++ (showStm (n+1) els)
showStm n (While cond body) = (indent n) ++ "while " ++ (show cond) ++ " do\n" ++ (showStm (n+1) body)
showStm n (Block decs deps body@(Block _ _ _)) = (indent n) ++ "begin\n" ++ s_decs ++ s_deps ++ (showStm (n+1) body) ++ "\n" ++ (indent n) ++ "end" where
  s_decs = concat $ map (\(var, exp) -> (indent (n+1)) ++ "var " ++ var ++ " := " ++ (show exp) ++ ";\n") decs
  s_deps = concat $ map (\(name, body) -> (indent (n+1)) ++ "proc " ++ name ++ " is\n" ++ (showStm (n+1) body) ++ ";\n") deps
showStm n (Block decs deps body) = (indent n) ++ "begin\n" ++ s_decs ++ s_deps ++ (showStm (n+1) body) ++ "\n" ++ (indent n) ++ "end" where
  s_decs = concat $ map (\(var, exp) -> (indent (n+1)) ++ "var " ++ var ++ " := " ++ (show exp) ++ ";\n") decs
  s_deps = concat $ map (\(name, body) -> (indent (n+1)) ++ "proc " ++ name ++ " is\n" ++ (showStm (n+2) body) ++ ";\n") deps
showStm n (Call name) = (indent n) ++ "call " ++ name
  
instance Show Stm where
  show = showStm 0

----------------------------------------
-- Parser
----------------------------------------

eatSpace :: Parser ()
eatSpace = Lexer.space (void spaceChar) lineComment blockComment where
  lineComment = Lexer.skipLineComment "//"
  blockComment = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme eatSpace

symbol :: String -> Parser String
symbol = lexeme . string

parenthesised :: Parser a -> Parser a
parenthesised = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme Lexer.integer

semicolon :: Parser String
semicolon = symbol ";"

word :: String -> Parser ()
word w = string w *> notFollowedBy alphaNumChar *> eatSpace

keywords :: [String]
keywords = ["skip", "begin", "end", "if", "then", "else", "call", "while", "do", "var", "proc", "true", "false"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check) where
  p = (:) <$> letterChar <*> many alphaNumChar
  check x = if x `elem` keywords
            then fail $ "keyword " ++ show x ++ " is reserved and so cannot be an identifier"
            else pure x

-----------------------

aexp :: Parser Aexp
aexp = makeExprParser terms ops where
  ops = [ [InfixR (Mult <$ symbol "*") ]
        , [InfixR (Add  <$ symbol "+")  
          ,InfixR (Sub  <$ symbol "-") ]
        ]
  terms =  try (N <$> lexeme Lexer.integer)
       <|> try (V <$> identifier)
       <|> parenthesised aexp

bexp :: Parser Bexp
bexp = makeExprParser terms ops where
  ops = [ [Prefix (Neg <$ symbol "!") ]
        , [InfixR (And <$ symbol "&") ]
        ]
  terms =  try (TRUE <$ (word "true"))
       <|> try (FALSE <$ (word "false"))
       <|> try (Le <$> aexp <*> (symbol "<=" *>) aexp)
       <|> try (Eq <$> aexp <*> (symbol "=" *>) aexp)
       <|> parenthesised bexp
        
decv :: Parser DecV
decv = many ((,) <$> (word "var" *>) identifier <*> (symbol ":=" *>) aexp <* semicolon) 

decp :: Parser DecP
decp = many ((,) <$> (word "proc" *>) identifier <*> (word "is" *>) stm <* semicolon)

stm :: Parser Stm
stm =  try (Skip <$ word "skip")
   <|> try (Ass <$> identifier <*> (symbol ":=" *>) aexp)
   <|> try (Call <$> (word "call" *>) identifier)
   <|> try (If <$> (word "if" *>) bexp <*> (word "then" *>) compound <*> (word "else" *>) compound)
   <|> try (While <$> (word "while" *>) bexp <*> (word "do" *>) compound)
   <|> try (between (word "begin") (word "end") (Block <$> decv <*> decp <*> compound))
   <|> parenthesised compound

compound :: Parser Stm
compound =  try (Comp <$> stm <*> (semicolon *>) compound)
        <|> stm

program :: Parser Stm
program = between eatSpace eof compound

-- For submission
parse :: String -> Stm
parse x = either (const Skip) id (MP.parse program "" x)

--------------------------------------------------
-- Denotational Semantics
--------------------------------------------------

type T = Bool
type Z = Integer
type State = Var -> Z

evalA :: Aexp -> State -> Z
evalA (N n) _ = n
evalA (V v) sigma = sigma v
evalA (Mult lhs rhs) sigma = (evalA lhs sigma) * (evalA rhs sigma)
evalA (Add lhs rhs) sigma = (evalA lhs sigma) + (evalA rhs sigma)
evalA (Sub lhs rhs) sigma = (evalA lhs sigma) - (evalA rhs sigma)

evalB :: Bexp -> State -> T
evalB TRUE _ = True
evalB FALSE _ = False
evalB (Neg x) sigma = not (evalB x sigma)
evalB (And lhs rhs) sigma = (evalB lhs sigma) && (evalB rhs sigma)
evalB (Le lhs rhs) sigma = (evalA lhs sigma) <= (evalA rhs sigma)
evalB (Eq lhs rhs) sigma = (evalA lhs sigma) == (evalA rhs sigma)

--------------------------------------------------
-- Natural Semantics
--------------------------------------------------
newtype EnvP = EnvP (Pname -> (Stm, EnvP))
type EnvV = Var -> Z
data Env = Env {sigma :: EnvV, tau :: Pname -> Stm}
blankEnv :: Env
blankEnv = Env (const undefined) (const undefined)

sub :: Eq a => (a -> b) -> a -> b -> (a -> b)
sub f x y x' = if x' == x then y else f x'

subV :: Env -> Var -> Z -> Env
subV (Env sigmaf tauf) var val = Env sigmaf' tauf where
  sigmaf' var' = if var == var' then val else sigmaf var'

defProc :: (Pname, Stm) -> Env -> Env
defProc (pname, body) (Env sigmaf tauf) = Env sigmaf tauf' where
  tauf' pname' = if pname' == pname then body else tauf pname'

evalS :: Stm -> Env -> Env
evalS Skip env = env
evalS (Ass var exp) env = subV env var (evalA exp $ sigma env)
evalS (Comp a b) env = let env' = (evalS a env) in evalS b env'
evalS (If cond thn els) env = if (evalB cond $ sigma env) then evalS thn env else evalS els env
evalS (While cond body) env = if (evalB cond $ sigma env) then evalS (While cond body) env' else env where
  env' = (evalS body env)
evalS (Block decv decp body) env = evalS body env'' where
  env' = evalr . (map toAss) $ decv
    where evalr = foldr evalS env
          toAss = uncurry Ass
  env'' = foldr defProc env' decp
evalS (Call p) s = evalS (tau s p) s

--------------------------------------------------
-- Variable Renaming
--------------------------------------------------


--------------------------------------------------
-- Submission
--------------------------------------------------
--s_static :: Stm -> State -> State
s_dynamic :: Stm -> State -> State
s_dynamic stm st = sigma $ evalS stm (Env st (const undefined))
--s_mixed :: Stm -> State -> State

--------------------------------------------------
-- Test programs
--------------------------------------------------
valof :: Var -> String -> Maybe Z
valof var src = (\s -> s var) <$> (\x -> s_dynamic x (const 0)) <$> parseMaybe program src

yval :: String -> Maybe Z
yval = valof "y"

simple_call :: String
simple_call = "y:=1; \n\
              \begin \n\
              \    proc fac is \n\
              \        skip; \n\
              \    skip \n\
              \end;\n\
              \call fac;\n\
              \y:= 10;\n\
              \call fac\n"
                      

fac_loop :: String
fac_loop = "/*fac loop (p.23)*/ \n\
            \y:=1; \n\
            \x := 5; \n\
            \(while !(x=1) do \n\
            \    y:=y*x; \n\
            \    x:=x-1 \n\
            \);\n\
            \skip\n"

fac_call :: String
fac_call = "//fac call (p.55) \n\
           \begin \n\
           \    proc fac is \n\
           \    begin \n\
           \        var z:=x; \n\
           \        if x=1 then \n\
           \            skip \n\
           \        else (\n\
           \            x:=x-1; \n\
           \            call fac; \n\
           \            y:=z*y \n\
           \        )\n\
           \    end; \n\
           \    y := 1; \n\
           \    x := 5; \n\
           \    call fac \n\
           \end\n"

fac_call_fixed :: String
fac_call_fixed = "x := 5;\n\
                 \y := 1; \n\
                 \begin \n\
                 \    proc fac is \n\
                 \    begin \n\
                 \        if x = 1 then \n\
                 \            skip \n\
                 \        else (\n\
                 \            y := y * x; \n\
                 \            x := x - 1; \n\
                 \            call fac \n\
                 \        )\n\
                 \    end; \n\
                 \    call fac \n\
                 \end"

scope_test :: String
scope_test = "//scope test (p.53) \n\
             \begin\n\
             \    var x:=0;\n\
             \    proc p is x:=x*2;\n\
             \    proc q is call p;\n\
             \    begin\n\
             \        var x:=5;\n\
             \        proc p is x:=x+1;\n\
             \        call q\n\
             \    end\n\
             \end\n"
