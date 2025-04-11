module Utils where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower)
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Node.FS.Aff (mkdir, readdir)
import Node.Path (FilePath)
import Node.Process (lookupEnv)

defaultTemplateFolder :: String
defaultTemplateFolder = "./template"

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

type RawFormattedMarkdownData =
  { frontMatter ::
      { title :: String
      , date :: String
      , slug :: String
      , tags :: Array String
      , status :: String
      }
  , content :: String
  , raw :: String
  }

type FormattedMarkdownData =
  { frontMatter :: { title :: String, date :: String, slug :: String, tags :: Array String, status :: Status }
  , content :: String
  , raw :: String
  }

type Category =
  { category :: String
  , posts :: Array String
  }

foreign import md2RawFormattedData :: String -> RawFormattedMarkdownData

md2FormattedData :: String -> FormattedMarkdownData
md2FormattedData s =
  let
    r = md2RawFormattedData s

    status = stringToStatus r.frontMatter.status
  in
    { frontMatter: { title: r.frontMatter.title, date: r.frontMatter.date, slug: r.frontMatter.slug, tags: r.frontMatter.tags, status: status }, content: r.content, raw: r.raw }

foreign import getCategoriesJson :: String -> Array Category

data Status
  = Draft
  | Published
  | InvalidStatus

-- Derive instances you need (Show, Eq, etc.)
derive instance eqStatus :: Eq Status

derive instance ordStatus :: Ord Status

instance showStatus :: Show Status where
  show Draft = "Draft"
  show Published = "Published"
  show InvalidStatus = "InvalidStatus"

stringToStatus :: String -> Status
stringToStatus s = case toLower s of
  "draft" -> Draft
  "published" -> Published
  _ -> InvalidStatus

type Config = { templateFolder :: String, outputFolder :: String, contentFolder :: String, newPostTemplate :: String, totalRecentPosts :: Int }

askConfig :: Aff Config
askConfig = liftEffect $ do
  _templateFolder <- lookupEnv "TEMPLATE_DIR"
  _outputFolder <- lookupEnv "OUTPUT_DIR"
  _contentFolder <- lookupEnv "POSTS_DIR"
  _totalRecentPosts <- do
    rp <- lookupEnv "RECENT_POSTS"
    pure $ rp >>= fromString
  pure $ { templateFolder: fromMaybe defaultTemplateFolder _templateFolder, outputFolder: fromMaybe defaultOutputFolder _outputFolder, contentFolder: fromMaybe defaultContentFolder _contentFolder, newPostTemplate: defaultBlogpostTemplate (fromMaybe defaultTemplateFolder _templateFolder), totalRecentPosts: fromMaybe defaultTotalRecentPosts _totalRecentPosts }

defaultTotalRecentPosts :: Int
defaultTotalRecentPosts = 5