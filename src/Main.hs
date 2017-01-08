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
                    Right () -> liftIO $ putStrLn "ok!"
                    Left err -> liftIO $ putStrLn err

            Bad err -> liftIO $ putStrLn $ "error:  " ++ show err
