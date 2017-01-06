{-# LANGUAGE LambdaCase #-}

module Main where

import ErrM
import ParLatte
import LexLatte

import Utils.Position
import Utils.Show

import System.Environment

main :: IO ()
main = getArgs >>= \case
    [filename] -> do
        fileContent <- readFile filename
        case pProgram (myLexer fileContent) of
            Ok program -> putStrLn $ "OK " ++ fullShow program
            Bad err -> putStrLn $ "error:  " ++ show err
