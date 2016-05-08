
module Blockchain.Output where

import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BC
import System.Log.FastLogger

printLogMsg::Loc->LogSource->LogLevel->LogStr->IO ()
printLogMsg loc logSource level msg = do
    putStrLn $ BC.unpack $ fromLogStr msg

