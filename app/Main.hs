{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.IO.Class
import System.Exit
import System.Environment
import Data.List

import System.Console.Haskeline

import Language.Foma
import Kip.Parser
import Kip.AST
import Kip.Eval

import Paths_kip (version)
import Data.Version (showVersion)

data ReplState =
  ReplState
    { ctx :: [String]
    }

main :: IO ()
main = 
  lookupEnv "TRMORPH" >>= \case
    Nothing -> die "The TRMORPH environment variable does not exist.\nTRMORPH ortam değişkeni mevcut değil."
    Just path -> do
      fsm <- fsmReadBinaryFile path
      
      -- TODO check if a file is passed in, then run the file instead of opening REPL

      -- Start REPL
      putStrLn $ "Kip " ++ showVersion version ++ "\n=============" 
      runInputT defaultSettings (loop fsm)
   where
    loop :: FSM -> InputT IO ()
    loop fsm = do
      minput <- getInputLine "Kip> "
      case minput of
          Nothing -> return ()
          Just ":çık" -> return ()
          Just ":quit" -> return ()
          Just input 
            | Just word <- stripPrefix ":name " input -> do 
                liftIO (ups fsm word) >>= \xs -> mapM_ outputStrLn xs
                loop fsm
            | Just word <- stripPrefix ":up " input -> do 
                liftIO (ups fsm word) >>= \xs -> mapM_ outputStrLn xs
                loop fsm
            | otherwise -> do 
                let pst = MkParserState fsm []
                liftIO (parseFromRepl pst input) >>= \case
                  Left err -> outputStrLn $ "Err: " ++ show err
                  Right stmt -> do
                    outputStrLn $ show stmt
                    liftIO (runEvalM (replStmt stmt) emptyEvalState) >>= \case
                      Left evalErr -> outputStrLn $ "Eval err: " ++ show evalErr
                      Right (res, st) ->
                        return ()
                loop fsm
