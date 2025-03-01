module Utils where

import Prelude
import Data.Either (Either(..))
import Data.String (toLower)
import Effect.Aff (Aff, try)
import Node.FS.Aff (mkdir, readdir)

templatesFolder :: String
templatesFolder = "./templates"

htmlOutputFolder :: String
htmlOutputFolder = "./public"

rawContentsFolder :: String
rawContentsFolder = "./contents"

blogpostTemplate :: String
blogpostTemplate = templatesFolder <> "/post.html"

newPostTemplate :: String
newPostTemplate = templatesFolder <> "/post.md"

tmpFolder :: String
tmpFolder = "./.vaak"

archiveTemplate :: String
archiveTemplate = templatesFolder <> "/archive.html"

homepageTemplate :: String
homepageTemplate = templatesFolder <> "/index.html"

createFolderIfNotPresent :: String -> Aff Unit
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

foreign import htmlToMarkdown :: String -> String

foreign import getEnv :: String -> String

foreign import getCategoriesJson :: Unit -> Array Category

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
