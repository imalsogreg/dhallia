{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Dhallia.Interpreter.Repl where

import qualified Control.Monad.IO.Class         as IO
import qualified Control.Monad.Reader           as Monad
import qualified Data.IORef                     as IORef
import qualified Data.List                      as List
import qualified Data.Text                      as Text
import qualified Dhall
import qualified Dhall.Core                     as Dhall
import qualified Dhall.Map                      as Map
import qualified Dhall.Pretty                   as Dhall
import qualified Network.HTTP.Client            as HTTP
import qualified Network.HTTP.Client.TLS        as HTTP
import qualified System.Console.Haskeline       as R
import qualified System.Console.Repline         as R

import           Dhallia.API
import           Dhallia.Cache
import           Dhallia.Cache.InMemory
import           Dhallia.Interpreter.HTTPClient
import  Dhallia.Prelude (inputExpr)
import           Dhallia.Expr (Expr)


data Env = Env
  { manager       :: HTTP.Manager
  , apis          :: IORef.IORef (Map.Map Text.Text (API (Cache IO)))
  , localBindings :: IORef.IORef (Map.Map Text.Text Expr)
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
      dhallInput <- IO.liftIO $ inputExpr (Text.pack apiArgument)
      Just r <- IO.liftIO $ Monad.runReaderT (runRequests api dhallInput) (manager env)
      IO.liftIO (print $ Dhall.prettyExpr r)
      return ()



options :: [(String, [String] -> Repl ())]
options =
  [("load", loadAPIs)
  ,("eval", evalExpr)
  ,("bind", addBinding)
  ,("list", listAPIs)
  ,("quit", error "Bye")
  ]


loadAPIs :: [String] -> Repl ()
loadAPIs [expr] = do
  apisRef  <- Monad.asks apis
  newAPIs  <- IO.liftIO $ inputExpr (Text.pack expr)
  newAPIs' <- IO.liftIO $ getAPIs makeInMemory newAPIs
  case newAPIs of
    Dhall.RecordLit e -> IO.liftIO $ IORef.modifyIORef apisRef (newAPIs' <>) >> putStrLn "Success"


listAPIs :: [String] -> Repl ()
listAPIs [] = do
  apisRef <- Monad.asks apis
  apis    <- IO.liftIO $ IORef.readIORef apisRef
  IO.liftIO $ mapM_ (putStrLn . Text.unpack) (Map.keys apis)


prettyBindings :: Map.Map Dhall.Text Expr -> Dhall.Text
prettyBindings bindings | List.null (Map.toList bindings) = ""
prettyBindings bindings = Text.unlines (bindingLines <> ["in\n"])
  where
    bindingLines      = map bindingLine (Map.toList bindings)
    bindingLine (k,v) = Text.unwords ["let", k, "=", Text.pack (show (Dhall.prettyExpr v))]


evalExpr :: [String] -> Repl ()
evalExpr exprText = do
  bindings <- Monad.asks localBindings >>= IO.liftIO . IORef.readIORef
  IO.liftIO $ print $ prettyBindings bindings
  expr <- inputExpr $ prettyBindings bindings <> Text.pack (unwords exprText)
  IO.liftIO (print $ Dhall.prettyExpr expr)


addBinding :: [String] -> Repl ()
addBinding [] = IO.liftIO $ putStrLn "usage: `:bind x { xFoo = 10, myBar = True }`"
addBinding (v:exprText) = do
  bindings <- Monad.asks localBindings
  bindingsText <- prettyBindings <$> IO.liftIO (IORef.readIORef bindings)
  IO.liftIO $ print bindingsText
  expr <- inputExpr $ bindingsText <> Text.pack (unwords exprText)
  IO.liftIO $ IORef.modifyIORef bindings (Map.insert (Text.pack v) expr)


completer :: R.CompleterStyle (Monad.ReaderT Env IO)
completer = R.Cursor $ \beforeWord (Text.pack -> word) -> do

  case (beforeWord, Text.stripPrefix ":" word) of

    -- Handle ":some-command"
    ("", Just _) -> return $
      completeFrom True word (map (Text.pack . (':':) . fst) options) -- [":load", ":list", ":quit"]

    -- Handle "some-api"
    _ -> do
      apis <- IO.liftIO . IORef.readIORef =<< Monad.asks apis
      return $ completeFrom False word (Map.keys apis)

  where
    completeFrom isDone prefix possibleCompletions =
        [ R.Completion { R.replacement = Text.unpack k
                       , R.display     = Text.unpack k
                       , R.isFinished  = isDone
                       }
        | k <- possibleCompletions
        , prefix `Text.isPrefixOf` k
        ]


ini :: Repl ()
ini = IO.liftIO (print "Welcome!")

type Repl a = R.HaskelineT (Monad.ReaderT Env IO) a

repl :: IO ()
repl = do
  mgr   <- HTTP.newTlsManager
  apis  <- IORef.newIORef mempty
  binds <- IORef.newIORef mempty
  Monad.runReaderT
    (R.evalRepl
      (pure "❀> ") cmd options (Just ':') completer ini
    ) (Env mgr apis binds)
