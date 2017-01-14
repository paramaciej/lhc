{-# LANGUAGE LambdaCase #-}

module Main where

import ErrM
import ParLatte
import LexLatte

import Utils.Position
import Utils.Show
import Utils.Abstract as A
import Utils.ToAbstract
import Utils.Types
import Utils.Verbose

import Utils.Eval
import Quattro.Generator
import Quattro.Types
import Quattro.Validator

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import Data.Maybe
import System.Environment



main :: IO ()
main = getArgs >>= \case
    ["-v", filename] -> runWithOptions filename True runCompiler
    [filename] -> runWithOptions filename False runCompiler
    _ -> usage
  where
    runCompiler :: CompilerOptsM ()
    runCompiler = do
        fileContent <- asks sourceFilename >>= lift . readFile
        case pProgram (myLexer fileContent) of
            Ok program -> do
                verbosePrint $ "Parsed source:\n" ++ fullShow program

                programValid (toA program) >>= \case
                    Right () -> do
                        liftIO $ putStrLn $ green "Program validated."
                        let simple = simplifyProgram (toA program)
                        verbosePrint $ "Simplified:\n" ++ show simple

                        runExceptT (generateValidatedQuattro simple) >>= \case
                            Right x -> verbosePrint $ show x
                            Left err -> liftIO $ putStrLn err
                    Left err -> liftIO $ putStrLn err

            Bad err -> liftIO $ putStrLn $ red "Parser error: " ++ show err
    usage = do
        putStrLn $ green "The Large Hadron Collider (or Latte (written in Haskell) Compiler)"
        progName <- getProgName
        putStrLn $ "usage: ./" ++ progName ++ " [-v] filename"
        putStrLn "\n '-v' option turns on more verbose output"