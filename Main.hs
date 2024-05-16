module Main where

import System.Environment
import System.IO

import GeneratedParser.ParEspresso

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      input <- readFile file
      case pProgram (myLexer input) of
        Left err -> hPutStrLn stderr err
        Right parsedObj -> putStrLn $ show parsedObj
    _ -> hPutStrLn stderr "Usage: ./interpreter <program>"