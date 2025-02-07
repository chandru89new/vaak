module TumblrFetch (main) where

import Prelude
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Parallel (parTraverse_)
import Data.Argonaut.Decode (fromJsonString)
import Data.Either (Either(..), either)
import Data.List (List)
import Data.String (Pattern(..), Replacement(..), replaceAll, splitAt)
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_, try)
import Effect.Class.Console (log)
import Fetch (Response, fetch)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Utils (createFolderIfNotPresent, getEnv, htmlToMarkdown, rawContentsFolder)

type FileData
  = { date :: String, title :: String, contents :: String, slug :: String }

writeDataToFile' :: FileData -> ExceptT Error Aff Unit
writeDataToFile' fd@{ slug } =
  ExceptT
    $ do
        res <- try $ writeTextFile UTF8 outputFilePath (fileDataToMarkdown fd)
        _ <- case res of
          Left err -> log $ constructError err
          Right _ -> log $ constructSuccess
        pure res
  where
  outputFilePath = (rawContentsFolder <> "/" <> slug <> ".md")

  constructError :: Error -> String
  constructError err = "Could not write " <> outputFilePath <> " because: " <> show err

  constructSuccess :: String
  constructSuccess = "Wrote: " <> outputFilePath <> "."

getPostsFromTumblr :: Int -> Int -> Aff Response
getPostsFromTumblr offset limit = fetch (requestURL <> "&limit=" <> show limit <> "&offset=" <> show offset) { "headers": { "Accept": "application/json" } }

requestURL :: String
requestURL = "https://api.tumblr.com/v2/blog/notesfromdruchan.tumblr.com/posts/text?api_key=" <> getEnv "API_KEY"

type APIResponse
  = { response ::
        { posts :: List { summary :: String, slug :: String, body :: String, date :: String }
        }
    }

fetchAndDecode :: Int -> Int -> Aff (Either String (List FileData))
fetchAndDecode offset limit = do
  { text, ok, statusText } <- getPostsFromTumblr offset limit
  t <- text
  case ok of
    true -> pure $ fromJsonString t # either (show >>> Left) (extractPosts >>> Right)
    false -> pure $ Left (statusText <> "\n" <> t)

extractPosts :: APIResponse -> List FileData
extractPosts = map extractPost <<< _.response.posts

extractPost :: { date :: String, summary :: String, slug :: String, body :: String } -> FileData
extractPost { summary, slug, body, date } = { title: summary, slug, contents: body, date: splitAt 10 date # _.before }

main :: Int -> Effect Unit
main offset =
  launchAff_
    $ do
        res <- fetchAndDecode offset 100
        ensureFolder <- runExceptT $ createFolderIfNotPresent rawContentsFolder
        case ensureFolder, res of
          Right _, Right fds -> parTraverse_ (writeDataToFile' >>> runExceptT) fds
          Left err, _ -> log $ show err
          _, Left err -> log err

fileDataToMarkdown :: FileData -> String
fileDataToMarkdown { title, contents, date, slug } =
  "---\n"
    <> "title: \""
    <> sanitizeTitle title
    <> "\"\n"
    <> "date: "
    <> date
    <> "\n"
    <> "slug: "
    <> slug
    <> "\n"
    <> "---\n"
    <> htmlToMarkdown contents

sanitizeTitle :: String -> String
sanitizeTitle = replaceAll (Pattern "\"") (Replacement "\'")
