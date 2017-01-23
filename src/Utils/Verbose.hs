{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Utils.Verbose where

import Control.Monad.Reader
import System.IO

data CompilerOpts = CompilerOpts
    { sourceFilename :: FilePath
    , verbose :: Bool
    }

type CompilerOptsM = ReaderT CompilerOpts IO

verbosePrint :: (MonadIO m, MonadReader CompilerOpts m) => String -> m ()
verbosePrint str = asks verbose >>= \case
        True -> liftIO $ hPutStrLn stderr str
        False -> return ()

runWithOptions :: String -> Bool -> CompilerOptsM () -> IO ()
runWithOptions filename isVerbose compilerOptsM = runReaderT compilerOptsM (CompilerOpts filename isVerbose)

indentStr :: String -> String
indentStr = unlines . map ("    " ++ ) . lines