module Utils where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array (elem, last)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.String (take, length, toLower)
import Data.String.CodeUnits (toCharArray)
import Effect.Aff (Aff, Error, try)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Foreign (Foreign, unsafeToForeign)
import Node.FS.Aff (mkdir, readdir)
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import Types (Config, FormattedMarkdownData, RawFormattedMarkdownData, Status(..), AppM, FrontMatterS)

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
  , siteUrl: fromMaybe "" config.domain
  , siteName: fromMaybe "" config.siteName
  }

prepareIndexContext :: Config -> Array FrontMatterS -> Foreign
prepareIndexContext config posts = unsafeToForeign
  { allPosts: map formatPost posts
  , siteUrl: fromMaybe "" config.domain
  , siteName: fromMaybe "" config.siteName
  }
  where
  formatPost fm = { title: fm.title, date: formatDate "MMM DD, YYYY" fm.date, slug: fm.slug }

prepareArchiveContext :: Config -> Array { year :: Int, posts :: Array FrontMatterS } -> Foreign
prepareArchiveContext config groupedPosts = unsafeToForeign
  { postsByYear: map formatGroup groupedPosts
  , siteUrl: fromMaybe "" config.domain
  , siteName: fromMaybe "" config.siteName
  }
  where
  formatGroup g = { year: g.year, posts: map formatPost g.posts }
  formatPost fm = { title: fm.title, date: formatDate "MMM DD, YYYY" fm.date, slug: fm.slug }

prepare404Context :: Config -> Foreign
prepare404Context config = unsafeToForeign { siteUrl: fromMaybe "" config.domain, siteName: fromMaybe "" config.siteName }

md2FormattedData :: String -> FormattedMarkdownData
md2FormattedData s =
  let
    r = md2RawFormattedData s
    status = stringToStatus r.frontMatter.status
  in
    { frontMatter: { title: r.frontMatter.title, date: r.frontMatter.date, slug: r.frontMatter.slug, tags: r.frontMatter.tags, status: status }, content: r.content, raw: r.raw }

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