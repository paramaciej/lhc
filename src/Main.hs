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
                        liftIO $ putStrLn $ "simplified:\n" ++ show (simplifyProgram (toA program))
                    Left err -> liftIO $ putStrLn err

            Bad err -> liftIO $ putStrLn $ red "Parser error: " ++ show err
