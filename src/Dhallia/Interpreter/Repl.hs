module Dhallia.Interpreter.Repl where

import qualified Control.Monad.Reader    as Monad
import qualified Control.Monad.IO.Class  as IO
import qualified Network.HTTP.Client     as HTTP
import qualified Data.Text               as Text
import           Data.Text               (Text)
import qualified Network.HTTP.Client.TLS as HTTP
import qualified System.Console.Repline  as R
import qualified Data.IORef              as IORef
import qualified Dhall.Map as Map
import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Pretty as Dhall

import qualified Data.List as List

import Dhallia.API
import Dhallia.Interpreter.HTTPClient

data Env = Env
  { manager :: HTTP.Manager
  , apis    :: IORef.IORef (Map.Map Text.Text API)
    -- cache :: DhalliaCache -- TODO
  }

cmd :: String -> Repl ()
cmd c = do
  let (apiName, apiArgument) = List.break (== ' ') c
  env  <- Monad.ask
  apis <- IO.liftIO (IORef.readIORef (apis env))
  case Map.lookup (Text.pack apiName) apis of
    Nothing  -> error ("No such api: " <> apiName)
    Just api -> do
      dhallInput <- IO.liftIO $ Dhall.inputExpr (Text.pack apiArgument)
      Just r <- IO.liftIO $ Monad.runReaderT (runRequests api dhallInput) (manager env)
      IO.liftIO (print $ Dhall.prettyExpr r)
      return ()

                      

options :: [(String, [String] -> Repl ())]
options =
  [("load", loadAPIs)
  ,("list", listAPIs)
  ,("quit", error "Bye")
  ]

loadAPIs :: [String] -> Repl ()
loadAPIs [expr] = do
  apisRef <- Monad.asks apis
  newAPIs <- IO.liftIO $ Dhall.inputExpr (Text.pack expr)
  case newAPIs of
    Dhall.RecordLit e -> IO.liftIO $ IORef.modifyIORef apisRef (<> getAPIs newAPIs) >> putStrLn "Success"

listAPIs :: [String] -> Repl ()
listAPIs [] = do
  apisRef <- Monad.asks apis
  apis    <- IO.liftIO $ IORef.readIORef apisRef
  IO.liftIO $ mapM_ (putStrLn . Text.unpack) (Map.keys apis)

completer :: Monad m => R.LineCompleter m
completer = undefined

ini :: Repl ()
ini = IO.liftIO (print "Welcome!")

type Repl a = R.HaskelineT (Monad.ReaderT Env IO) a

repl :: IO ()
repl = do
  mgr  <- HTTP.newTlsManager
  apis <- IORef.newIORef mempty
  Monad.runReaderT
    (R.evalRepl
      (pure "❀> ") cmd options (Just ':') (R.Cursor completer) ini
    ) (Env mgr apis)

