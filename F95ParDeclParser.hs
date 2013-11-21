{- 
Craig Cuthbertson 1002386
Functional Programming 4
Parsing, Code Generation and State Manipulation in Haskell: a Real-world Application
21/11/13
This code correctly parses a parameter declaration
-}
module F95ParDeclParser 
where
import F95Types
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import F95VarDeclParser

-- parse a parameter declaration string into a ParDecl 
f95_par_decl_parser :: Parser ParDecl
--f95_par_decl_parser = return $ MkParDecl dummyVarType [] dummyVarName dummyExpr
f95_par_decl_parser = do 
	vtype <- type_parser
	char ','
	whiteSpace
	symbol "parameter"
	whiteSpace
	symbol "::"
	whiteSpace
	v <- word
	whiteSpace
	char '='
	whiteSpace
	c <- expr_parser
	return $ MkParDecl vtype [] v c