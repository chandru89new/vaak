module Cache where

import Prelude

import Control.Monad.Reader (ask)
import Control.Parallel (parTraverse)
import Data.Array (filter, mapMaybe)
import Data.Either (Either(..), either, hush)
import Data.Map (Map, empty, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, joinWith, split)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Node.Buffer (toString)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, writeTextFile)
import Node.Path (FilePath)
import Types (Config, AppM)
import Utils (getConfig, liftAppM)

-- createNewCacheLookup :: Array U.FormattedMarkdownData -> Aff Unit
-- createNewCacheLookup fds = do
--   statsAsArray <- statAllToString slugs
--   ExceptT $ try $ writeTextFile UTF8 "./cache" (joinWith "\n" statsAsArray)
--   where
--   -- slugs = map (\fd -> fd.frontMatter.slug) fds
--   slugs = [ "trust-systems", "use-interval-hook" ]
getStat :: FilePath -> String -> Aff ({ filename :: String, stat :: String })
getStat contentsFolder filename = do
  buf <- try $ liftEffect $ execSync ("stat -f \"%Sm %Sc\" -n " <> contentsFolder <> "/" <> filename) defaultExecSyncOptions
  case buf of
    Left _ -> pure $ { filename, stat: "" }
    Right buffer -> do
      b <- try $ liftEffect $ toString UTF8 buffer
      case b of
        Right s -> pure { filename, stat: s }
        Left _ -> pure { filename, stat: "" }

getStatAll :: Config -> Array String -> Aff (Array { filename :: String, stat :: String })
getStatAll config filenames = do
  parTraverse (getStat config.contentFolder) filenames

writeCacheData :: AppM Unit
writeCacheData = do
  cacheData <- createCacheData
  _ <- liftAppM $ writeTextFile UTF8 "./.cache" cacheData
  pure unit

createCacheData :: AppM String
createCacheData = do
  config <- ask
  liftAppM $ do
    contents <- try $ readdir config.contentFolder
    unwrapped <- pure $ either (\_ -> []) (filter (contains (Pattern ".md"))) contents
    stats <- getStatAll config unwrapped
    pure $ joinWith "\n" (map toString' stats)
  where
  toString' { filename, stat } = filename <> "::" <> stat

readCacheData :: Aff CacheData
readCacheData = do
  cacheContents <- (try $ readTextFile UTF8 "./.cache") >>= (\v -> pure $ hush v)
  case cacheContents of
    Nothing -> pure empty
    Just cacheData -> pure $ (split (Pattern "\n") >>> map (\line -> split (Pattern "::") line) >>> fn >>> fromFoldable) cacheData
  where
  fn = mapMaybe toTuple
  toTuple [ k, v ] = Just $ Tuple k v
  toTuple _ = Nothing

type CacheData = Map String String

needsInvalidation :: CacheData -> String -> Aff Boolean
needsInvalidation cacheData filename = do
  case lookup filename cacheData of
    Nothing -> pure true
    Just x -> do
      config <- getConfig
      cd <- getStat config.contentFolder filename
      pure $ cd.stat /= x
-- test =
--   launchAff_
--     $ do
--         res <- getStat "aaji"
--         log $ show res
