module Main where

import System.Environment
import System.IO

import GeneratedParser.ParEspresso
import TypeChecker

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            input <- readFile file
            case pProgram (myLexer input) of
                Left err -> hPutStrLn stderr err
                Right parsedObj -> case typeCheck parsedObj of
                    Left err -> hPutStrLn stderr err
                    Right _ -> putStrLn "Program type checks"
        _ -> hPutStrLn stderr "Usage: ./interpreter <program>"