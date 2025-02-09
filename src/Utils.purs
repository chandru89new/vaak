module Utils where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (Aff, error, throwError, try)
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

type FormattedMarkdownData
  = { frontMatter :: { title :: String, date :: String, slug :: String, tags :: Array String, ignore :: Boolean }
    , content :: String
    , raw :: String
    }

type Category
  = { category :: String
    , posts :: Array String
    }

foreign import md2FormattedData :: String -> FormattedMarkdownData

foreign import htmlToMarkdown :: String -> String

foreign import getEnv :: String -> String

foreign import getCategoriesJson :: Unit -> Array Category
