{- 
Craig Cuthbertson 1002386
Functional Programming 4
Parsing, Code Generation and State Manipulation in Haskell: a Real-world Application
21/11/13
This code does not currently do anything
-}
module OpenCLAPIGenerator (
    gen_OpenCL_API_calls
        ) where
import F95Types
import Text.Regex.Posix -- suggest use of regular expressions
import Data.Char
import qualified Data.Map as H (lookup)

import System.Process -- only for localtime, entirely optional
import System.IO.Unsafe (unsafePerformIO) -- only for localtime, entirely optional

gen_OpenCL_API_calls :: ArgTable -> [String] -> [String] -> [String] -> String -> [String]    
gen_OpenCL_API_calls ocl_args arg_names const_arg_names src_lines templ_src_name = []
gen_OpenCL_API_calls ocl_args arg_names const_arg_names src_lines templ_src_name = do
	--Buffers
	

get_c_type :: VarType -> String
get_c_type vt = ""
                
ucfirst (x:xs)  = (toUpper x):xs

localtime = unsafePerformIO $ readProcess "/bin/date" [] []

