{- 
Craig Cuthbertson 1002386
Functional Programming 4
Parsing, Code Generation and State Manipulation in Haskell: a Real-world Application
21/11/13
This code was given
-}
module F95Types (
    module F95VarDecl
    ,module F95ParDecl
    ,module F95SubDecl
    ,ArgTable
    ,dummyVarType 
    ,dummyRange 
    ,dummyIntent 
    ,dummyArgMode 
    ,dummyVarDecl 
    ,dummyConstExpr 
    ,dummyVarExpr 
    ,dummyExpr 
    ,dummyVarName 
        )
    where

import F95SubDecl
import F95VarDecl
import F95ParDecl

import Data.Map
-- a table with as key the argument name and as value the corresponding VarDecl record.
type ArgTable = Data.Map.Map String VarDecl


dummyVarType = MkVarType F95Integer 0
dummyRange =  MkRange (Const 0) (Const 0)
dummyIntent = InOut
dummyArgMode = ReadWrite
dummyVarDecl = MkVarDecl dummyVarType [] dummyIntent [] dummyArgMode True

dummyConstExpr = Const 0
dummyVarExpr = Var "DUMMY"
dummyExpr = dummyConstExpr

dummyVarName = "DUMMY"
