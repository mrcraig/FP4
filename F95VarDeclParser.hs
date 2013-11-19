module F95VarDeclParser where
import F95Types
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.Regex.Posix

-- Run a parser p on a string str and print the result
run_parser_print :: Show a => Parser a -> String -> IO ()
run_parser_print p str = do
      case parse p "" str of
           Left err -> do
               putStr "parse error at "
               print err
           Right x  -> putStrLn $ "    "++(show x)++","
                                                                                                                                                         
-- Run a parser p on a string str and return the result
run_parser :: Parser a -> String -> a
run_parser p str =  case parse p "" str of
    Left err -> error $ "parse error at " ++ (show err)
    Right val  -> val  

f95_var_decl_parser :: Parser VarDecl
f95_var_decl_parser = return dummyVarDecl
{-f95_var_decl_parser = do
  whiteSpace
  vtype <- try(type_parser) <|> dummyVarType
  comma
  dimension <- dim_parser
  comma
  intentdef <- intent_parser
  symbol "::"
  varlist <- arglist_parser
  symbol "!$ACC"
  argmode <- ocl_argmode_parser-}
  
-- TODO may be broken      
type_parser :: Parser VarType
type_parser = do
  varclass <- stringLiteral
  varkind <- integer
  if varclass =~ "integer"
    then return $ MkVarType F95Integer varkind
    else return $ MkVarType F95Real varkind

{-get_type :: String -> NumType
get_type stringlit
  | stringlit == "integer" = return $ F95Integer
  | stringlit == "real" = return $ F95Real
  | otherwise = error "Type must be Integer or Real"-}
      
dim_parser :: Parser [Range]
dim_parser = return [dummyRange]

range_parser :: Parser Range
range_parser = return dummyRange

-- ie v
single_var_range :: Parser Range    
single_var_range = do
  vrange <- var_expr
  return $ MkRange vrange vrange

-- ie 5
single_const_range :: Parser Range
single_const_range = do
  crange <- const_expr
  return $ MkRange crange crange

-- ie kp+2
single_expr_range :: Parser Range
single_expr_range = do
  erange <- expr_parser
  return $ MkRange erange erange

-- ie 0:ip+2
range_expr :: Parser Range    
range_expr =  do
  st <- expr_parser
  char ':'
  en <- expr_parser
  return $ MkRange st en 

intent_parser :: Parser Intent    
intent_parser = do
  symbol "intent("
  intent_tmp <- stringLiteral
  char ')'
  return $ parse_intent intent_tmp

parse_intent :: String -> Intent
parse_intent intent_str
  | intent_str == "in" = In
  | intent_str == "out" = Out
  | intent_str == "inout" = InOut
  | otherwise = InOut

   
arglist_parser :: Parser [VarName]    
arglist_parser = return [dummyVarName]

ocl_argmode_parser :: Parser OclArgMode    
ocl_argmode_parser = return dummyArgMode

-- Parser for a term in expression as used e.g. in the dimension() attribute. 
-- This is not a dummy
-- I don't know why this is here
term :: Parser Expr
term = parens expr_parser <|> const_expr <|> var_expr <?> "simple expression"
      
-- Parser for an expression as used e.g. in the dimension() attribute. 
-- This is not a dummy
expr_parser :: Parser Expr
expr_parser = buildExpressionParser optable term <?> "expression"

-- parser for a constant, e.g. 42
const_expr :: Parser Expr
const_expr = do
  c <- integer
  return $ Const c

-- parser for a variable e.g. v
var_expr :: Parser Expr
var_expr =  do
  e <- identifier
  return $ Var e

-- I suggest you don't touch the code below. It is not dummy code.
optable =
    let
        binop name assoc   = Infix ( do {  reservedOp name; return (\x y ->(Op (MkOpExpr name x y))) } ) assoc
        prefix name     = Prefix  ( reservedOp  name >> return (\x ->(Pref (MkPrefixOpExpr name x))) ) 
    in
        [ 
          [ prefix "-" ],
          [ binop "*"  AssocLeft, binop "/"  AssocLeft, binop "%" AssocLeft ]
        , [ binop "+"  AssocLeft, binop "-"  AssocLeft ]
        ]

lexer       = P.makeTokenParser emptyDef    

parens          = P.parens lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
word            = P.identifier lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer    
comma           = P.comma lexer
semi            = P.semi lexer
natural         = P.natural lexer