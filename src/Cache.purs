module Cache where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array (filter, find, (!!))
import Data.Either (Either(..), either, hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), contains, joinWith, replace, split)
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Node.Buffer (toString)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, writeTextFile)
import Node.Path (FilePath)
import Utils (askConfig)

-- createNewCacheLookup :: Array U.FormattedMarkdownData -> Aff Unit
-- createNewCacheLookup fds = do
--   statsAsArray <- statAllToString slugs
--   ExceptT $ try $ writeTextFile UTF8 "./cache" (joinWith "\n" statsAsArray)
--   where
--   -- slugs = map (\fd -> fd.frontMatter.slug) fds
--   slugs = [ "trust-systems", "use-interval-hook" ]
getStat :: FilePath -> String -> Aff ({ slug :: String, stat :: String })
getStat contentsFolder slug = do
  buf <- try $ liftEffect $ execSync ("stat -f \"%Sm %Sc\" -n " <> contentsFolder <> "/" <> slug <> ".md") defaultExecSyncOptions
  case buf of
    Left _ -> pure $ { slug, stat: "" }
    Right buffer -> do
      b <- try $ liftEffect $ toString UTF8 buffer
      case b of
        Right s -> pure { slug, stat: s }
        Left _ -> pure { slug, stat: "" }

getStatAll :: Array String -> Aff (Array { slug :: String, stat :: String })
getStatAll slugs = do
  config <- askConfig
  parTraverse (getStat config.contentFolder) slugs

writeCacheData :: Aff Unit
writeCacheData = do
  cacheData <- createCacheData
  _ <- try $ writeTextFile UTF8 "./.cache" cacheData
  pure unit

createCacheData :: Aff String
createCacheData = do
  config <- askConfig
  contents <- try $ readdir config.contentFolder
  unwrapped <- pure $ either (\_ -> []) (filter (contains (Pattern ".md")) >>> (map $ replace (Pattern ".md") (Replacement ""))) contents
  stats <- getStatAll unwrapped
  pure $ joinWith "\n" (map toString' stats)
  where
  toString' { slug, stat } = slug <> "::" <> stat

getCacheValue :: String -> String -> Maybe String
getCacheValue cacheData slug =
  ( split (Pattern "\n")
      >>> map (\line -> split (Pattern "::") line)
      >>> find
        ( \splitLine -> case splitLine !! 0 of
            Nothing -> false
            Just s -> s == slug
        )
      >>> map (\found -> fromMaybe "" $ found !! 1)
  )
    cacheData

readCacheData :: Aff String
readCacheData = do
  cacheContents <- try $ readTextFile UTF8 "./.cache"
  pure $ fromMaybe "" $ hush cacheContents

needsInvalidation :: String -> String -> String -> Boolean
needsInvalidation existingCacheData newCacheData slug =
  let
    oldCacheValue = getCacheValue existingCacheData slug

    newCacheValue = getCacheValue newCacheData slug
  in
    case oldCacheValue, newCacheValue of
      Just ocv, Just ncv -> ocv /= ncv
      _, _ -> true

-- test =
--   launchAff_
--     $ do
--         res <- getStat "aaji"
--         log $ show res
