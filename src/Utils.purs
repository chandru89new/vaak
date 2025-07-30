module Utils where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array (last)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (take, length, toLower)
import Data.String.CodeUnits (toCharArray)
import Effect.Aff (Aff, Error, try)
import Effect.Class (class MonadEffect, liftEffect)
import Node.FS.Aff (mkdir, readdir)
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import Types (Category, Config, FormattedMarkdownData, RawFormattedMarkdownData, Status(..), AppM)

defaultTemplateFolder :: String
defaultTemplateFolder = "./templates"

defaultOutputFolder :: String
defaultOutputFolder = "./public"

defaultContentFolder :: String
defaultContentFolder = "./posts"

defaultBlogpostTemplate :: FilePath -> String
defaultBlogpostTemplate templateFolder = templateFolder <> "/post.html"

newPostTemplate :: FilePath -> String
newPostTemplate templateFolder = templateFolder <> "/post.md"

tmpFolder :: String
tmpFolder = "./.vaak"

archiveTemplate :: FilePath -> String
archiveTemplate templateFolder = templateFolder <> "/archive.html"

homepageTemplate :: FilePath -> String
homepageTemplate templateFolder = templateFolder <> "/index.html"

createFolderIfNotPresent :: FilePath -> Aff Unit
createFolderIfNotPresent folderName = do
  res <- try $ readdir folderName
  case res of
    Right _ -> pure $ unit
    Left _ -> mkdir folderName

foreign import formatDate :: String -> String -> String

foreign import md2RawFormattedData :: String -> RawFormattedMarkdownData

md2FormattedData :: String -> FormattedMarkdownData
md2FormattedData s =
  let
    r = md2RawFormattedData s
    status = stringToStatus r.frontMatter.status
  in
    { frontMatter: { title: r.frontMatter.title, date: r.frontMatter.date, slug: r.frontMatter.slug, tags: r.frontMatter.tags, status: status }, content: r.content, raw: r.raw }

foreign import getCategoriesJson :: String -> Array Category

getConfig :: forall m. (MonadEffect m) => m Config
getConfig = liftEffect $ do
  templateFolder <- lookupEnv "TEMPLATE_DIR" >>= (pure <$> fromMaybe defaultTemplateFolder)
  outputFolder <- lookupEnv "OUTPUT_DIR" >>= (pure <$> fromMaybe defaultOutputFolder)
  contentFolder <- lookupEnv "POSTS_DIR" >>= (pure <$> fromMaybe defaultContentFolder)
  totalRecentPosts <- lookupEnv "RECENT_POSTS" >>= (pure <$> fn)
  domain <- lookupEnv "SITE_URL" >>= (\v -> pure $ (dropLeadingSlash <$> v))
  pure $ { domain: domain, templateFolder: templateFolder, outputFolder: outputFolder, contentFolder: contentFolder, blogPostTemplate: defaultBlogpostTemplate templateFolder, totalRecentPosts: totalRecentPosts }
  where
  fn :: Maybe String -> Int
  fn x = fromMaybe defaultTotalRecentPosts $ (x >>= fromString)

defaultTotalRecentPosts :: Int
defaultTotalRecentPosts = 5

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