module EvalExpr (eval, VarTable) where
import F95Types
import Data.Maybe 
import qualified Data.Map as H

type VarTable = H.Map String Expr
-- given an expression and the variable lookup table, return the integer value of the evaluated expression and the updated table
-- expr = k+1?
eval :: Expr -> VarTable -> (Integer, VarTable)
--eval expr vtable = (0,H.empty)

-- Deal with OpExpr's by calling eval_expr
eval (Op o) vtable = do
	let val = eval_expr o vtable
	(val, vtable)

-- Deal with variables by lookuping up value in vtable
eval (Var v) vtable = do
	let lookupMaybe = H.lookup v vtable
	let look = fromJust lookupMaybe
	let val = fst(eval look vtable)
	let upd = Const val
	let chtab = H.insert v upd vtable
	(val, vtable)


-- Deal with prefix by calling eval_prefix_expr
eval (Pref p) vtable = do
	let v = eval_prefix_expr p vtable
	(v, vtable)

-- Closing condition
eval(Const c) vtable = do
	(c,vtable)

prod_maybe :: Expr -> Maybe Expr
prod_maybe ex = Just ex


-- given a binary operator expression (e.g. x+y) and the variable lookup table, return the integer value of the evaluated expression
eval_expr :: OpExpr -> VarTable -> Integer    
eval_expr oe vt = do
	let oper = oe_op oe
	let rhs = oe_rhs oe
	let lhs = oe_lhs oe
	let rhsval = fst(eval rhs vt)
	let lhsval = fst(eval lhs vt)
	case oper of
		"+" ->  lhsval + rhsval
		"-" ->  lhsval - rhsval
		--"*" ->  lhsval * rhsval
		--"/" ->  lhsval / rhsval
		"%" ->  lhsval `mod` rhsval

-- given a unary operator expression (e.g. -x) and the variable lookup table, return the integer value of the evaluated expression
eval_prefix_expr :: PrefixOpExpr -> VarTable -> Integer
eval_prefix_expr pe vt = do
	let pre = poe_op pe
	let ep = poe_exp pe
	case pre of
		"+" -> fst(eval ep vt)
		--"-" -> fst(eval (ep * (-1)) vt)