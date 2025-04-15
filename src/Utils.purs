module Utils where

import Prelude

import Control.Monad.List.Trans (catMaybes)
import Data.Array (init, last, (!!), length)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith, toLower)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Regex (Regex, split)
import Effect.Aff (Aff, try)
import Effect.Class (class MonadEffect, liftEffect)
import Node.FS.Aff (mkdir, readdir)
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import Types (Category, Config, FormattedMarkdownData, RawFormattedMarkdownData, Status(..))

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

askConfig :: forall m. (MonadEffect m) => m Config
askConfig = liftEffect $ do
  templateFolder <- lookupEnv "TEMPLATE_DIR" >>= (pure <$> fromMaybe defaultTemplateFolder)
  outputFolder <- lookupEnv "OUTPUT_DIR" >>= (pure <$> fromMaybe defaultOutputFolder)
  contentFolder <- lookupEnv "POSTS_DIR" >>= (pure <$> fromMaybe defaultContentFolder)
  totalRecentPosts <- lookupEnv "RECENT_POSTS" >>= (pure <$> fn)
  domain <- lookupEnv "SITE_URL" >>= (pure <$> cleanupDomain)
  pure $ { domain: domain, templateFolder: templateFolder, outputFolder: outputFolder, contentFolder: contentFolder, blogPostTemplate: defaultBlogpostTemplate templateFolder, totalRecentPosts: totalRecentPosts }
  where
  fn :: Maybe String -> Int
  fn x = fromMaybe defaultTotalRecentPosts $ (x >>= fromString)

  cleanupDomain :: Maybe String -> String
  cleanupDomain Nothing = "https://my.blog"
  cleanupDomain (Just x) = dropLeadingSlash x

defaultTotalRecentPosts :: Int
defaultTotalRecentPosts = 5

-- Helper functions
stringToStatus :: String -> Status
stringToStatus s = case toLower s of
  "draft" -> Draft
  "published" -> Published
  _ -> InvalidStatus s

endsWith :: Char -> String -> Boolean
endsWith ch str =
  let
    asArray = toCharArray str
    lastChar = asArray !! (length asArray - 1)
  in case lastChar of
    Just c -> c == ch
    _ -> false

dropLeadingSlash :: String -> String
dropLeadingSlash str = 
  if endsWith '/' str then fromMaybe "" go else str 
  where
  go = map fromCharArray $ init (toCharArray str) 