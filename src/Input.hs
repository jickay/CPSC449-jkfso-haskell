module Input where

import System.Environment
import System.IO

input :: [String] -> IO String
input args = do
    -- Get lines from input file name (arg 1)
    contents <- getLines (args !! 0)
    return contents

output args contents = do
    -- Write to output file name (arg 2)
    writeFile (args !! 1) contents
    

getLines :: FilePath -> IO String
getLines fileName = readFile fileName