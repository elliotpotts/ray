import Parser
import Syntax
import Hfix
                    
-- Needs refactoring badly
parseInteger : Parser Integer
parseInteger = failure

parseIdent : Parser String
parseIdent = (skipWs *>) (strCons <$> alpha <*> consecutive isAlphaNum)

parseALit : Parser AExpr
parseALit = [| ALiteral' parseInteger |]

parseAIdent : Parser AExpr
parseAIdent = [| AIdent' parseIdent |]

parseAPres0 : Parser AExpr
parseAPres1 : Parser AExpr
parseAPres2 : Parser AExpr

parseAPres0 =  parseALit
           <|> parseAIdent
           <|> (tok "(" *> parseAPres2 <* tok ")")

parseAPres1 =  (Factor'   <$> parseAPres0 <*> (tok "*" *> parseAPres1))
           <|> (Quotient' <$> parseAPres0 <*> (tok "/" *> parseAPres1))
           <|> parseAPres0

parseAPres2 =  (Sum'        <$> parseAPres1 <*> (tok "+" *> parseAPres2))
           <|> (Difference' <$> parseAPres1 <*> (tok "-" *> parseAPres2))
           <|> parseAPres1

parseAExpr : Parser AExpr
parseAExpr = parseAPres2

parseBTrue : Parser BExpr
parseBTrue = tok "true" *> produce BTrue'

parseBFalse : Parser BExpr
parseBFalse = tok "false" *> produce BFalse'

parseBPres0 : Parser BExpr
parseBPres1 : Parser BExpr
parseBPres2 : Parser BExpr
parseBPres3 : Parser BExpr

parseBPres0 =  parseBTrue
           <|> parseBFalse
           <|> (tok "(" *> parseBPres3 <* tok ")")

parseBPres1 =  (EqTest'  <$> parseAExpr <*> (tok "==" *> parseAExpr))
           <|> (LTETest' <$> parseAExpr <*> (tok "<=" *> parseAExpr))
           <|> parseBPres0

parseBPres2 =  (Negation' <$> (tok "!" *> parseBPres2))
           <|> parseBPres1

parseBPres3 =  (Conjunction' <$> parseBPres2 <*> (tok "&&" *> parseBPres3))
           <|> parseBPres2

parseBExpr : Parser BExpr
parseBExpr = parseBPres3

parseStmt0 : Parser Stmt
parseStmt1 : Parser Stmt

parseStmt0 =  (Assign' <$> parseIdent <*> (tok "=" *> parseAExpr))
          <|> (tok "skip" *> (produce Skip'))
          <|> (WhileDo' <$> (tok "while" *> parseBExpr) <*> (tok "do" *> parseStmt0))
          <|> (IfThenElse' <$> (tok "if" *> parseBExpr) <*> (tok "then" *> parseStmt0) <*> (tok "else" *> parseStmt0))
          <|> ((tok "{") *> parseStmt1 <* (tok "}"))

parseStmt1 = (Then' <$> parseStmt0 <*> (tok ";" *> parseStmt1))
          <|> parseStmt0

parseStmt : Parser Stmt
parseStmt = parseStmt1

test : String
test = "z = x;" ++
       "while(two <= y) do {" ++
       "    z = z * x;" ++
       "    y = y - one" ++
       "}"


main : IO ()
main = do let result = parse test parseStmt
          putStrLn (show (length result))
          pure ()
