module Utils where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (toLower)
import Effect.Aff (Aff, try)
import Effect.Class (class MonadEffect, liftEffect)
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
  { frontMatter :: RawFrontMatter
  , content :: String
  , raw :: String
  }

type FrontMatter a =
  { title :: String
  , date :: String
  , slug :: String
  , tags :: Array String
  , status :: a
  }

type FormattedMarkdownData =
  { frontMatter :: FrontMatterS
  , content :: String
  , raw :: String
  }

type RawFrontMatter = FrontMatter String
type FrontMatterS = FrontMatter Status

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
  | InvalidStatus String

-- Derive instances you need (Show, Eq, etc.)
derive instance eqStatus :: Eq Status

derive instance ordStatus :: Ord Status

instance showStatus :: Show Status where
  show Draft = "Draft"
  show Published = "Published"
  show (InvalidStatus s) = "InvalidStatus" <> show s

stringToStatus :: String -> Status
stringToStatus s = case toLower s of
  "draft" -> Draft
  "published" -> Published
  _ -> InvalidStatus s

type Config = { templateFolder :: String, outputFolder :: String, contentFolder :: String, blogPostTemplate :: String, totalRecentPosts :: Int }

askConfig :: forall m. (MonadEffect m) => m Config
askConfig = liftEffect $ do
  templateFolder <- lookupEnv "TEMPLATE_DIR" >>= (pure <$> fromMaybe defaultTemplateFolder)
  outputFolder <- lookupEnv "OUTPUT_DIR" >>= (pure <$> fromMaybe defaultOutputFolder)
  contentFolder <- lookupEnv "POSTS_DIR" >>= (pure <$> fromMaybe defaultContentFolder)
  totalRecentPosts <- lookupEnv "RECENT_POSTS" >>= (pure <$> fn)
  pure $ { templateFolder: templateFolder, outputFolder: outputFolder, contentFolder: contentFolder, blogPostTemplate: defaultBlogpostTemplate templateFolder, totalRecentPosts: totalRecentPosts }
  where
  fn :: Maybe String -> Int
  fn x = fromMaybe defaultTotalRecentPosts $ (x >>= fromString)

defaultTotalRecentPosts :: Int
defaultTotalRecentPosts = 5