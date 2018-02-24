module Main where

import Input

import System.Environment

main = do
    -- Store arguments in arg
    args <- getArgs
    -- Get input
    contents <- input args
    print contents
    let linesOfFile = lines contents
    print (unlines linesOfFile)
    
    -- Solution filler
    let solution = "Solution: A B C D E F G H; Quality: 0"

    -- Print output file
    -- (Solution will be final string to print out)
    output args solution


getALine :: [String] -> String
getALine (aLine:linesOfFile) = show aLine

