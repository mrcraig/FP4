{- 
Craig Cuthbertson 1002386
Functional Programming 4
Parsing, Code Generation and State Manipulation in Haskell: a Real-world Application
21/11/13
This code correctly reads and writes files
-}
module F95SrcIO (
		read_F95_src,
        write_F95_src
) where
import Data.Char (toLower)
-- Fortran is case-insensitive so turn everything into lowercase
lc = map toLower
-- given the name of the file, read it into a list of strings, one per line of source code
read_F95_src :: String -> IO [String]
read_F95_src src_name = do
	contents <- readFile src_name
	let output = lines (lc contents)
	return output

-- given a list of strings, one per line of source code, and the name of the file, write the strings to the file
write_F95_src :: String -> [String] -> IO ()
write_F95_src src_name src_lines =  writeFile src_name (unlines src_lines)