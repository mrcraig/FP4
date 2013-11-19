module F95OpenACCParser (
    extract_OpenACC_regions_from_F95_src
) where
import Text.Regex.Posix -- suggest use of regular expressions

-- given the source code as a list of lines (strings), extract the OpenACC regions for Arguments and ConstArguments as well as the parameter declarations, and return them as a tuple of three lists of strings, in that order.
extract_OpenACC_regions_from_F95_src :: [String] -> ([String],[String],[String])
extract_OpenACC_regions_from_F95_src in_src_lines = (extract_args in_src_lines,extract_constargs in_src_lines,extract_params in_src_lines)

extract_args :: [String] -> [String]
extract_args [] = []
extract_args (x:xs)
	| x =~ "!\\$acc arguments" :: Bool = extract_args_in xs
	| otherwise = extract_args xs

extract_args_in :: [String] -> [String]
extract_args_in [] = error "ACC Arguments not closed"
extract_args_in (x:xs)
	| x =~ "!\\$acc end arguments" :: Bool = extract_args xs
	| otherwise = [x] ++ extract_args_in xs

extract_constargs :: [String] -> [String]
extract_constargs [] = []
extract_constargs (x:xs)
	| x =~ "!\\$acc constarguments" :: Bool = extract_constargs_in xs
	| otherwise = extract_constargs xs

extract_constargs_in :: [String] -> [String]
extract_constargs_in [] = error "ACC ConstArguments not closed"
extract_constargs_in (x:xs)
	| x =~ "!\\$acc end constarguments" :: Bool = extract_constargs xs
	| otherwise = [x] ++ extract_constargs_in xs

extract_params :: [String] -> [String]
extract_params [] = []
extract_params (x:xs)
	| x =~ "parameter" :: Bool = [x] ++ extract_params xs
	| otherwise = extract_params xs