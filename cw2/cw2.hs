import Prelude hiding (product, sum, Num)
import Control.Monad
import Text.Megaparsec hiding (parse, State)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Lexer as Lexer
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Data.List hiding (product, sum)
import Data.Bool

--------------------------------------------------
-- Syntax
--------------------------------------------------

type Num = Integer
type Var = String
type Pname = String
type Dec1V = (Var, Aexp)
type Dec1P = (Pname, Stm)
type DecV = [Dec1V]
type DecP = [Dec1P]

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

evalA :: State -> Aexp -> Z
evalA _ (N n) = n
evalA sigma (V v) = sigma v
evalA sigma (Mult lhs rhs) = (evalA sigma lhs) * (evalA sigma rhs)
evalA sigma (Add lhs rhs) = (evalA sigma lhs) + (evalA sigma rhs)
evalA sigma (Sub lhs rhs) = (evalA sigma lhs) - (evalA sigma rhs)

evalB :: State -> Bexp -> T
evalB _ TRUE = True
evalB _ FALSE = False
evalB sigma (Neg x) = not (evalB sigma x)
evalB sigma (And lhs rhs) = (evalB sigma lhs) && (evalB sigma rhs)
evalB sigma (Le lhs rhs) = (evalA sigma lhs) <= (evalA sigma rhs)
evalB sigma (Eq lhs rhs) = (evalA sigma lhs) == (evalA sigma rhs)

--------------------------------------------------
-- Natural Semantics
--------------------------------------------------
type Loc = Z
data StoreReq = NewLoc | At Loc deriving Eq
type Store = Var -> StoreReq -> Z
type EnvV = Var -> Loc
newtype EnvP = EnvP (Pname -> (Stm, EnvV, EnvP, DecP))
type Env = (EnvV, Store, EnvP)

getenvv :: Env -> EnvV
getenvv (envv, _, _) = envv
getsto :: Env -> Store
getsto (_, sto, _) = sto
getenvp :: Env -> EnvP
getenvp (_, _, envp) = envp

subs :: Eq a => (a -> b) -> a -> b -> (a -> b)
subs f x' fx' x = if x == x' then fx' else f x

freshen :: Loc -> Loc
freshen = succ

toSt :: Store -> EnvV -> State
toSt sto envv var = (sto var) (At (envv var))

ass :: Store -> EnvV -> Var -> Z -> Store
ass sto envv var val = subs sto var (subs (sto var) (At loc) val) where loc = envv var

updp :: DecP -> EnvV -> EnvP -> EnvP
updp decp envv envp = foldr upd1p envp decp where
  upd1p (pname, s) envp@(EnvP envpf) = EnvP $ subs envpf pname (s, envv, envp, decp)

evalS :: Env -> Stm -> Env
evalS env Skip = env
evalS (envv, sto, envp) (Ass var exp) = (envv, ass sto envv var val, envp) where
  val = evalA (toSt sto envv) exp
evalS env (Comp s1 s2) = env'' where
  env' = evalS env s1
  env'' = evalS env' s2
evalS env@(envv, sto, envp) (If cond thn elz) = (evalS env) . (bool elz thn) . (evalB (toSt sto envv)) $ cond
evalS env@(envv, sto, envp) (While cond body) = (bool env env'') . (evalB (toSt sto envv)) $ cond where
  env' = evalS env body
  env'' = evalS env' (While cond body)
evalS env@(envv, sto, envp) (Block decv decp body) = (envv, sto'', envp) where
  (env'v, sto') = updv decv envv sto where
    updv :: DecV -> EnvV -> Store -> (EnvV, Store)
    updv [] envv sto = (envv, sto)
    updv ((var, aexp):xs) envv sto = updv xs env'v sto'' where
      loc = sto var NewLoc
      env'v = subs envv var loc
      val = evalA (toSt sto envv) aexp
      sto' = ass sto env'v var val
      sto'' = subs sto' var (subs (sto' var) NewLoc (freshen loc))
  env'p = updp decp env'v envp
  (env''v, sto'', env''p) = evalS (env'v, sto', env'p) body
evalS env@(envv, sto, envp@(EnvP envpf)) (Call pname) = (envv, sto', envp) where
  (pbody, penvv, penvp, decp) = envpf pname
  recEnvp = EnvP $ case penvp of
    EnvP penvpf -> subs penvpf pname (pbody, penvv, recEnvp, decp)
  (_, sto', _) = evalS (penvv, sto, (updp decp penvv recEnvp)) pbody
--------------------------------------------------
-- Submission
--------------------------------------------------
emptyEnv :: Env
emptyEnv = (envv, sto, EnvP envpf) where
  envv = const 0
  defaultStoVal (At l) = 0 -- maybe undefined?
  defaultStoVal NewLoc = 1
  sto = const defaultStoVal
  envpf _ =  (Skip, envv, EnvP envpf, [])

-- envFromState :: State -> Env
-- envFromState st = (envv, sto, EnvP envpf) where
--   envv = const 0
--   vsto (At l) = undefined
--   vsto NewLoc = 1
--   sto = const vsto
--   envpf = const (Skip, envv, EnvP envpf, [])

s_static :: Stm -> State -> State
s_static stm st = toSt sto envv where
  (envv, sto, envp) = evalS emptyEnv stm

--s_dynamic :: Stm -> State -> State
--s_dynamic stm st = sigma $ evalS stm (Env st (const undefined))
--s_mixed :: Stm -> State -> State

--------------------------------------------------
-- Test programs
--------------------------------------------------
valof :: Var -> String -> Maybe Z
valof var src = (\s -> s var) <$> (\x -> s_static x (const undefined)) <$> parseMaybe program src

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

fac_loop_blk :: String
fac_loop_blk = "/*fac loop (p.23)*/ \n\
               \begin\n\
               \    var y := 1; \n\
               \    var x := 5; \n\
               \    (while !(x=1) do \n\
               \        y:=y*x; \n\
               \        x:=x-1 \n\
               \    );\n\
               \    z := y \n\
               \end"

testrec :: String
testrec = "begin \n\
          \    proc rep is begin \n\
          \        if false then\n\
          \            x := 10 \n\
          \        else (\n\
          \            x := 0\n\
          \        )\n\
          \    end; \n\
          \    x := 4; \n\
          \    call rep \n\
          \end"

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
             \    var y:=0-90;\n\
             \    var x:=8;\n\
             \    proc p is x:=x*2;\n\
             \    proc q is call p;\n\
             \    begin\n\
             \        var x:=5;\n\
             \        proc p is x:=x+1;\n\
             \        call q;\n\
             \        y := x\n\
             \    end\n\
             \end\n"

small_scope_test :: String
small_scope_test = "begin\n\
                   \    var x := 10;\n\
                   \    proc p is x := x;\n\
                   \    begin\n\
                   \        var x := 20;\n\
                   \        call p\
                   \    end\n\
                   \end\n"

parity_nest :: String
parity_nest = "y := 2034; \n\
              \begin \n\
              \    proc iseven is begin \n\
              \        proc isodd is (\n\
              \            if y = 0 then \n\
              \                mod2 := 1 \n\
              \            else \n\
              \                y := y - 1; \n\
              \                call iseven \n\
              \        ); \n\
              \        if y = 0 then \n\
              \            mod2 := 0 \n\
              \        else \n\
              \            y := y - 1;\n\
              \            call isodd\n\
              \    end; \n\
              \    call iseven \n\
              \end"

parity_local :: String
parity_local = "begin \n\
               \    var y := 203; \n\
               \    proc isodd is begin \n\
               \        if y = 0 then \n\
               \            mod2 := 1 \n\
               \        else \n\
               \            y := y - 1; \n\
               \            call iseven \n\
               \    end; \n\
               \    proc iseven is begin \n\
               \        if y = 0 then \n\
               \            mod2 := 0 \n\
               \        else \n\
               \            y := y - 1; \n\
               \            call isodd \n\
               \    end; \n\
               \    call iseven \n\
               \end"

parity_scope :: String
parity_scope = "begin\n\
               \  var x := 102;\n\
               \  proc isEven is (\n\
               \    if x = 0 then \n\
               \      y := 0 \n\
               \    else \n\
               \      x := x - 1; \n\
               \      call isOdd \n\
               \  ); \n\
               \  proc isOdd is (\n\
               \    if x = 0 then \n\
               \      y := 1 \n\
               \    else \n\
               \      x := x - 1; \n\
               \      call isEven \n\
               \  ); \n\
               \  call isEven \n\
               \end"
               
mut_test :: String
mut_test = "begin\n\
           \    var y := 203; \n\
           \    proc iseven is begin \n\
           \        y := 1\n\
           \    end;\n\
           \    call iseven\n\
           \end"

mut_test2 :: String
mut_test2 = "begin var x:=10; \n\
            \  proc foo is (  \n\
            \    begin  \n\
            \      x:=x+1;   \n\
            \      call bar   \n\
            \    end   \n\
            \  );   \n\
            \  proc bar is (   \n\
            \    begin   \n\
            \      x:=x+2   \n\
            \    end   \n\
            \  );   \n\
            \  proc baz is ( \n\
            \    begin   \n\
            \      x:=x+3;   \n\
            \      call foo   \n\
            \    end   \n\
            \  );   \n\
            \  (   \n\
            \    call baz;   \n\
            \    y:=x   \n\
            \  )   \n\
            \end"

scope_test3 :: String
scope_test3 = "x := 10; \n\
              \begin \n\
              \    proc foo is y := x; \n\
              \    proc bar is begin \n\
              \        var x := 5; \n\
              \        call foo \n\
              \    end; \n\
              \    call foo \n\
              \end"
