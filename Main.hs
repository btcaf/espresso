module Main where

import System.Environment
import System.IO

import GeneratedParser.ParEspresso
import TypeChecker
import Interpreter

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
                    Right _ -> do 
                        interpretRes <- interpret parsedObj
                        case interpretRes of
                            Left err -> hPutStrLn stderr err
                            Right n -> hPutStrLn stderr $ "Program returned: " ++ show n
        _ -> hPutStrLn stderr "Usage: ./interpreter <program>"