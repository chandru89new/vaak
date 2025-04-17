module Logs where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

red :: String -> String
red s = "\x1b[31m" <> s <> "\x1b[0m"

green :: String -> String
green s = "\x1b[32m" <> s <> "\x1b[0m"

yellow :: String -> String
yellow s = "\x1b[33m" <> s <> "\x1b[0m"

blue :: String -> String
blue s = "\x1b[34m" <> s <> "\x1b[0m"

logSuccess :: forall m. (MonadEffect m) => String -> m Unit
logSuccess str = log $ green "[success]: " <> str

logInfo :: forall m. (MonadEffect m) => String -> m Unit
logInfo str = log $ blue "[info]: " <> str

logError :: forall m. (MonadEffect m) => String -> m Unit
logError str = log $ red "[error]: " <> str

logWarning :: forall m. (MonadEffect m) => String -> m Unit
logWarning str = log $ yellow "[warning]: " <> str
