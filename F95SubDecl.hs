{- 
Craig Cuthbertson 1002386
Functional Programming 4
Parsing, Code Generation and State Manipulation in Haskell: a Real-world Application
21/11/13
This code was given
-}
module F95SubDecl (
        SubDecl
        )
    where
import F95VarDecl

-- Datatype for subroutine signature. Not needed for the coursework.
data SubDecl = MkSubDecl {
	sd_name :: String
    ,sd_arglst :: [String]
    ,sd_argdecls :: [VarDecl]
    ,sd_code :: [String]
} deriving (Eq, Ord, Show)


