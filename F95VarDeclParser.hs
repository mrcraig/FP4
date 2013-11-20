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
--working! yay.
f95_var_decl_parser = do
  whiteSpace
  vtype <- type_parser
  optionMaybe (char ',')
  dimension <- option [dummyRange] (try dim_parser)
  intentdef <- option dummyIntent (try intent_parser)
  whiteSpace
  symbol "::"
  varlist <- arglist_parser
  optionMaybe (symbol "!$acc argmode ")
  argmode <- option dummyArgMode $ ocl_argmode_parser
  return $ MkVarDecl vtype dimension intentdef varlist argmode True
  
-- working
type_parser :: Parser VarType
type_parser = do
  whiteSpace
  varclass <- word
  varkind <- option 4 $ kind_parser
  if varclass =~ "integer"
    then return $ MkVarType F95Integer varkind
    else return $ MkVarType F95Real varkind

-- working, default size 4 if no kind specified
kind_parser :: Parser Integer
kind_parser = do
  whiteSpace
  symbol "(kind="
  varkin <- integer
  char ')'
  return varkin

--working      
dim_parser :: Parser [Range]
dim_parser = do
  whiteSpace
  symbol "dimension("
  rangelist <- commaSep1 range_parser
  char ')'
  return rangelist

--Works
range_parser :: Parser Range
range_parser = try range_expr <|> try single_expr_range <|> try single_const_range <|> try single_var_range <?> error "Could not parse range"

-- ie v (w)
single_var_range :: Parser Range    
single_var_range = do
  whiteSpace
  vrange <- var_expr
  return $ MkRange vrange vrange

-- ie 5 (w)
single_const_range :: Parser Range
single_const_range = do
  whiteSpace
  crange <- const_expr
  return $ MkRange crange crange

-- ie kp+2 (w)
single_expr_range :: Parser Range
single_expr_range = do
  whiteSpace
  erange <- expr_parser
  return $ MkRange erange erange

-- ie 0:ip+2 (w)
range_expr :: Parser Range    
range_expr =  do
  whiteSpace
  st <- expr_parser
  char ':'
  en <- expr_parser
  return $ MkRange st en 

--Working
intent_parser :: Parser Intent    
intent_parser = do
  whiteSpace
  symbol "intent("
  intent_tmp <- word
  char ')'
  return $ parse_intent intent_tmp

-- (w)
parse_intent :: String -> Intent
parse_intent intent_str
  | intent_str == "in" = In
  | intent_str == "out" = Out
  | intent_str == "inout" = InOut
  | otherwise = error "No valid intent provided"

-- t1,t2,t3 (w)
arglist_parser :: Parser [VarName]    
arglist_parser = do
  whiteSpace
  arglist <- commaSep1 word
  return arglist

--Working
ocl_argmode_parser :: Parser OclArgMode    
ocl_argmode_parser = do
  whiteSpace
  argmod <- word
  return $ select_argmode argmod 

-- (w)
select_argmode :: String -> OclArgMode
select_argmode argmode
  | argmode == "read" = Read 
  | argmode == "write" = Write 
  | argmode == "readwrite" = ReadWrite
  | otherwise = error "No valid argmode provided"

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
  whiteSpace
  c <- integer
  return $ Const c

-- parser for a variable e.g. v
var_expr :: Parser Expr
var_expr =  do
  whiteSpace
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