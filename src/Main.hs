{-# LANGUAGE LambdaCase #-}

module Main where

import ErrM
import ParLatte
import LexLatte

import Utils.Position
import Utils.Show
import Utils.Abstract as A
import Utils.Types

import Data.Map
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except
import System.Environment

main :: IO ()
main = getArgs >>= \case
    [filename] -> do
        fileContent <- readFile filename
        case pProgram (myLexer fileContent) of
            Ok program -> do
                putStrLn $ "OK " ++ fullShow program
                let aux (name, t) = do
                        putStrLn $ name ++ " : " ++ show t
                        putStrLn "Check..."

                mapM_ aux (toList $ topTypes (toA program))

                case programValid (toA program) of
                    Right () -> putStrLn "ok!"
                    Left err -> putStrLn err

            Bad err -> putStrLn $ "error:  " ++ show err
