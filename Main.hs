module Main where

import System.Environment
import System.IO
import System.Exit

import GeneratedParser.ParEspresso
import TypeChecker
import Interpreter

errorExit :: String -> IO ()
errorExit msg = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            input <- readFile file
            case pProgram (myLexer input) of
                Left err -> errorExit err
                Right parsedObj -> case typeCheck parsedObj of
                    Left err -> errorExit err
                    Right _ -> do 
                        interpretRes <- interpret parsedObj
                        case interpretRes of
                            Left err -> errorExit err
                            Right 0 -> exitWith ExitSuccess
                            Right n -> exitWith $ ExitFailure n
        _ -> hPutStrLn stderr "Usage: ./interpreter <program>"