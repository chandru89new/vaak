module Logs where

import Prelude

red :: String -> String
red s = "\x1b[31m" <> s <> "\x1b[0m"

green :: String -> String
green s = "\x1b[32m" <> s <> "\x1b[0m"

yellow :: String -> String
yellow s = "\x1b[33m" <> s <> "\x1b[0m"

blue :: String -> String
blue s = "\x1b[34m" <> s <> "\x1b[0m"

logSuccess str = green "[SUCCESS]: " <> str

logInfo str = blue "[INFO]: " <> str

logError str = red "[ERROR]: " <> str

logWarning str = yellow "[WARNING]: " <> str
