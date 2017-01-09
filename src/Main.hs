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
import Quattro

import Data.Map
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except
import System.Environment

import Control.Lens


main :: IO ()
main = getArgs >>= \case
    [filename] -> do
        fileContent <- readFile filename
        runWithOptions filename True $ case pProgram (myLexer fileContent) of
            Ok program -> do
                liftIO $ putStrLn $ "OK " ++ fullShow program

                programValid (toA program) >>= \case
                    Right () -> do
                        liftIO $ putStrLn "ok!"
                        let simple = simplifyProgram (toA program)
                        liftIO $ putStrLn $ "simplified:\n" ++ show simple

                        let initialSt = QuattroSt empty 0 (QuattroCode empty) 1 1
                        endSt <- execStateT (genProgram simple) initialSt
                        liftIO $ putStrLn $ "\n\nQUATTRO\n" ++ show endSt
                    Left err -> liftIO $ putStrLn err

            Bad err -> liftIO $ putStrLn $ red "Parser error: " ++ show err
