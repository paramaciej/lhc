{-# LANGUAGE LambdaCase #-}

module Main where

import ErrM
import ParLatte

import Asm.Generator
import Quattro.Alive
import Quattro.Types
import Quattro.Validator
import Utils.Eval
import Utils.Show
import Utils.ToAbstract
import Utils.Types
import Utils.Verbose

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M
import System.Environment
import System.FilePath.Posix
import System.Process



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
                        let simplified = simplifyProgram (toA program)
                        verbosePrint $ "Simplified:\n" ++ show simplified

                        runExceptT (generateValidatedQuattro simplified) >>= \case
                            Right x -> do
                                verbosePrint $ show x
                                let clear@(ClearProgram fs) = clearProgram x
                                let inSets = M.unions $ map calculateInSets $ M.elems fs

                                verbosePrint $ green "Cleared φ :\n" ++ show clear
                                verbosePrint $ green "Alive sets:\n" ++ unlines (map (\(label, ss) -> yellow (show label) ++ ": " ++ show ss) $ M.toList inSets)

                                code <- unlines . map show <$> genAsm clear

                                asmFile <- (`replaceExtension` "s") <$> asks sourceFilename
                                lift $ writeFile asmFile code
                                _ <- lift $ createProcess $ shell $ "gcc " ++ asmFile ++ " lib/runtime.o -static -o " ++ dropExtension asmFile
                                return ()


                            Left err -> liftIO $ putStrLn err
                    Left err -> liftIO $ putStrLn err

            Bad err -> liftIO $ putStrLn $ red "Parser error: " ++ show err
    usage = do
        putStrLn $ green "The Large Hadron Collider (or Latte (written in Haskell) Compiler)"
        progName <- getProgName
        putStrLn $ "usage: ./" ++ progName ++ " [-v] filename"
        putStrLn "\n '-v' option turns on more verbose output"