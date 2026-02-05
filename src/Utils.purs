module Utils where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array (concatMap, elem, foldl, last, nubBy, sortBy)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (toUnfoldable)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..), length, replaceAll, split, take, toLower)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, try)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign, unsafeToForeign)
import Node.ChildProcess (ExecSyncOptions)
import Node.FS.Aff (mkdir, readdir)
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import Types (AppM, Config, FormattedMarkdownData, FrontMatterS, RawFormattedMarkdownData, Status(..), Collection)

defaultOutputFolder :: String
defaultOutputFolder = "./public"

defaultContentFolder :: String
defaultContentFolder = "./posts"

templateFolder :: String
templateFolder = "./templates"

tmpFolder :: String
tmpFolder = "./.vaak"

createFolderIfNotPresent :: FilePath -> Aff Unit
createFolderIfNotPresent folderName = do
  res <- try $ readdir folderName
  case res of
    Right _ -> pure $ unit
    Left _ -> mkdir folderName

foreign import formatDate :: String -> String -> String

foreign import md2RawFormattedData :: String -> RawFormattedMarkdownData

-- Context preparation functions (pure PureScript)
preparePostContext :: Config -> FrontMatterS -> String -> Foreign
preparePostContext config fm content = unsafeToForeign
  { title: fm.title
  , date: formatDate "MMM DD, YYYY" fm.date
  , slug: fm.slug
  , content: content
  , collections: getUniqueCollections (Array.singleton fm)
  , siteUrl: fromMaybe "" config.domain
  , siteName: fromMaybe "" config.siteName
  }

prepareIndexContext :: Config -> Array FrontMatterS -> Foreign
prepareIndexContext config posts = unsafeToForeign
  { allPosts: map formatPost posts
  , siteUrl: fromMaybe "" config.domain
  , siteName: fromMaybe "" config.siteName
  , postsByYear: map (\yearPosts -> { year: yearPosts.year, posts: map formatPost yearPosts.posts }) $ groupPostsByYearArray posts
  , collections: getUniqueCollections posts
  }

prepareCollectionContext :: Config -> Collection -> Array FrontMatterS -> Foreign
prepareCollectionContext config collection posts = unsafeToForeign
  { siteUrl: fromMaybe "" config.domain
  , siteName: fromMaybe "" config.siteName
  , collection: unsafeToForeign collection
  , posts: map formatPost posts
  }

prepareArchiveContext :: Config -> Array { year :: Int, posts :: Array FrontMatterS } -> Foreign
prepareArchiveContext config groupedPosts = unsafeToForeign
  { postsByYear: map formatGroup groupedPosts
  , siteUrl: fromMaybe "" config.domain
  , siteName: fromMaybe "" config.siteName
  }
  where
  formatGroup g = { year: g.year, posts: map formatPost g.posts }

prepare404Context :: Config -> Foreign
prepare404Context config = unsafeToForeign { siteUrl: fromMaybe "" config.domain, siteName: fromMaybe "" config.siteName }

md2FormattedData :: String -> FormattedMarkdownData
md2FormattedData s =
  let
    r = md2RawFormattedData s
    status = stringToStatus r.frontMatter.status
  in
    { frontMatter: { title: r.frontMatter.title, date: r.frontMatter.date, slug: r.frontMatter.slug, collections: r.frontMatter.collections, status: status }, content: r.content, raw: r.raw }

getConfig :: forall m. (MonadEffect m) => m Config
getConfig = liftEffect $ do
  outputFolder <- lookupEnv "OUTPUT_DIR" >>= (pure <$> fromMaybe defaultOutputFolder)
  contentFolder <- lookupEnv "POSTS_DIR" >>= (pure <$> fromMaybe defaultContentFolder)
  domain <- lookupEnv "SITE_URL" >>= (\v -> pure $ (dropLeadingSlash <$> v))
  siteName <- lookupEnv "SITE_NAME"
  pure { domain, outputFolder, contentFolder, siteName }

stringToStatus :: String -> Status
stringToStatus s = case toLower s of
  "draft" -> Draft
  "unlisted" -> Unlisted
  "published" -> Published
  _ -> InvalidStatus s

endsWith :: Char -> String -> Boolean
endsWith ch str =
  let
    asArray = toCharArray str
  in
    fromMaybe false ((==) ch <$> (last asArray))

dropLeadingSlash :: String -> String
dropLeadingSlash str =
  if endsWith '/' str then take (length str - 1) str else str

runAppM :: forall a. Config -> AppM a -> Aff (Either Error a)
runAppM config app = runExceptT $ runReaderT app config

liftAppM :: forall a. Aff a -> AppM a
liftAppM aff = ReaderT \_ -> ExceptT $ try $ aff

fileNameExists :: String -> FilePath -> AppM Boolean
fileNameExists fileName path = liftAppM $ do
  contents <- readdir path
  pure $ elem (fileName <> ".md") contents

folderExists :: FilePath -> AppM Boolean
folderExists path = liftAppM $ do
  res <- try $ readdir path
  case res of
    Right _ -> pure true
    Left _ -> pure false

groupPostsByYearArray :: Array FrontMatterS -> Array { year :: Int, posts :: Array FrontMatterS }
groupPostsByYearArray posts =
  let
    grouped = groupPostsByYear posts
    asList = Map.toUnfoldable grouped # sortBy (\(Tuple a1 _) (Tuple a2 _) -> if a1 > a2 then LT else GT)
  in
    map (\(Tuple year ps) -> { year, posts: ps }) asList

groupPostsByYear :: Array (FrontMatterS) -> Map Int (Array (FrontMatterS))
groupPostsByYear posts = foldl foldFn Map.empty posts
  where
  foldFn :: (Map Int (Array (FrontMatterS))) -> (FrontMatterS) -> Map Int (Array (FrontMatterS))
  foldFn b a =
    let
      updateFn v = Just $ Array.snoc (fromMaybe [] v) a

      year = extractYear a.date
    in
      case year of
        Nothing -> b
        Just y -> Map.alter updateFn y b

  extractYear dateString =
    split (Pattern "-") dateString
      # Array.head
      # map (fromString)
      # join

defaultExecSyncOptions :: ExecSyncOptions
defaultExecSyncOptions =
  { cwd: Nothing
  , input: Nothing
  , appendStdio: Nothing
  , env: Nothing
  , gid: Nothing
  , killSignal: Nothing
  , maxBuffer: Nothing
  , shell: Nothing
  , timeout: Nothing
  , uid: Nothing
  , windowsHide: Nothing
  }

slugify :: String -> String
slugify str = toLower str # replaceAll (Pattern " ") (Replacement "-")

formatPost :: FrontMatterS -> { title :: String, date :: String, slug :: String, collections :: Array String }
formatPost fm = { title: fm.title, date: formatDate "MMM DD, YYYY" fm.date, slug: fm.slug, collections: fm.collections }

getUniqueCollections :: Array FrontMatterS -> Array Collection
getUniqueCollections ps = concatMap (\p -> p.collections) ps
  # Set.fromFoldable
  # toUnfoldable
  # map (\c -> { name: c, slug: slugify c })
  # nubBy (\a b -> if a.slug == b.slug then EQ else LT)
