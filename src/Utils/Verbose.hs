{-# LANGUAGE LambdaCase #-}
module Utils.Verbose where

import Control.Monad.Reader

data CompilerOpts = CompilerOpts
    { sourceFilename :: String
    , verbose :: Bool
    }

type CompilerOptsM = ReaderT CompilerOpts IO

verbosePrint :: String -> CompilerOptsM ()
verbosePrint str = asks verbose >>= \case
        True -> liftIO $ putStrLn str
        False -> return ()

runWithOptions :: String -> Bool -> CompilerOptsM () -> IO ()
runWithOptions filename isVerbose compilerOptsM = runReaderT compilerOptsM (CompilerOpts filename isVerbose)

indentStr :: String -> String
indentStr = unlines . map ("\t" ++ ) . lines