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
import System.Environment

main :: IO ()
main = getArgs >>= \case
    [filename] -> do
        fileContent <- readFile filename
        case pProgram (myLexer fileContent) of
            Ok program -> do
                putStrLn $ "OK " ++ fullShow program
                let aux (name, (td, t)) = putStrLn $ name ++ "\t (" ++ show (fromJust $ pos td) ++ ") \t: " ++ show t
                mapM_ aux (toList $ topTypes (toA program))
            Bad err -> putStrLn $ "error:  " ++ show err
