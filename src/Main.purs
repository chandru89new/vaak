module Main where

import Prelude

import Cache (CacheData, needsInvalidation', readCacheData)
import Cache as Cache
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes, drop, filter, find, foldl, head, sortBy, take)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), contains, joinWith, replaceAll, split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Logs as Logs
import Node.Buffer (Buffer)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, writeTextFile)
import Node.Process (argv, exit)
import Prelude as Maybe
import RssGenerator as Rss
import Utils (Config, FormattedMarkdownData, FrontMatterS, Status(..), archiveTemplate, askConfig, createFolderIfNotPresent, formatDate, getCategoriesJson, homepageTemplate, md2FormattedData, newPostTemplate, tmpFolder)
import Utils as U

main :: Effect Unit
main = do
  args <- argv
  cmd <- pure $ mkCommand args
  case cmd of
    Help -> log $ helpText
    ShowVersion -> log $ "v0.6.0"
    NewPost slug ->
      launchAff_
        $ do
            config <- askConfig
            res <- runExceptT $ createNewPost config slug
            case res of
              Left err -> do
                _ <- Logs.logError ("Could not create a new post: " <> show err)
                liftEffect $ exit 1
              Right _ -> Logs.logSuccess "Created new post. Happy writing!"
    Invalid -> do
      Logs.logError $ "Invalid command."
      log $ helpText
      exit 1
    Build ->
      launchAff_
        $ do
            config <- askConfig
            res <- runExceptT $ buildSite config
            _ <- try $ liftEffect $ execSync ("rm -rf " <> tmpFolder) defaultExecSyncOptions
            case res of
              Left err -> do
                Logs.logError $ "Error when building the site: " <> show err
                liftEffect $ exit 1
              Right _ -> Logs.logSuccess $ "Site built and available in the " <> config.outputFolder <> " folder."

helpText :: String
helpText =
  """Commands:
  help - print this help text.
  version - print version info.
  build - build the site.
  new [slug] - create a new post.
"""

buildSite :: Config -> ExceptT Error Aff Unit
buildSite config =
  ExceptT $ try
    $ do
        Logs.logInfo "Starting..."
        _ <- createFolderIfNotPresent tmpFolder
        postsMetadata <- generatePostsHTML
        Logs.logSuccess $ "Posts page generated."
        Logs.logInfo $ "Generating archive page..."
        _ <- writeArchiveByYearPage postsMetadata
        Logs.logSuccess $ "Archive page generated."
        Logs.logInfo $ "Generating home page..."
        _ <- createHomePage postsMetadata
        Logs.logSuccess $ "Home page generated."
        Logs.logInfo $ "Copying 404.html..."
        _ <- liftEffect $ execSync ("cp " <> config.templateFolder <> "/404.html " <> tmpFolder) defaultExecSyncOptions
        Logs.logSuccess $ "404.html copied."
        Logs.logInfo "Copying images folder..."
        _ <- liftEffect $ execSync ("cp -r " <> "./images " <> tmpFolder) defaultExecSyncOptions
        Logs.logSuccess $ "images folder copied."
        Logs.logInfo "Copying js folder..."
        _ <- liftEffect $ execSync ("cp -r " <> "./js " <> tmpFolder) defaultExecSyncOptions
        Logs.logSuccess "js folder copied."
        Logs.logInfo "Generating styles.css..."
        Logs.logInfo "This may take a while. I am installing (temporarily) TailwindCSS to generate the stylesheet."
        _ <- generateStyles
        Logs.logSuccess "styles.css generated."
        Logs.logInfo "Generating RSS feed..."
        _ <- Rss.generateRSSFeed (take 10 postsMetadata)
        Logs.logSuccess "RSS feed generated."
        _ <- cleanupNodeModules
        Logs.logInfo $ "Copying " <> tmpFolder <> " to " <> config.outputFolder
        _ <- createFolderIfNotPresent config.outputFolder
        _ <- liftEffect $ execSync ("cp -r " <> tmpFolder <> "/* " <> config.outputFolder) defaultExecSyncOptions
        Logs.logSuccess "Copied."
        Logs.logInfo "Updating cache..."
        _ <- Cache.writeCacheData
        Logs.logSuccess "Cached updated."

newtype Template = Template String

data Command
  = Build
  | ShowVersion
  | Help
  | NewPost String
  | Invalid

instance showCommand :: Show Command where
  show Build = "Build"
  show Help = "Help"
  show (NewPost _) = "NewPost"
  show Invalid = "Invalid"
  show ShowVersion = "ShowVersion"

mkCommand :: Array String -> Command
mkCommand xs = case head (drop 2 xs) of
  Just "version" -> ShowVersion
  Just "help" -> Help
  Just "build" -> Build
  Just "new" -> case head $ drop 3 xs of
    Just slug -> NewPost slug
    _ -> Invalid
  _ -> Invalid

getFilesAndTemplate :: Aff { files :: Array String, template :: String }
getFilesAndTemplate = do
  config <- askConfig
  files <- readdir config.contentFolder
  template <- readPostTemplate
  pure { files, template }

generatePostsHTML :: Aff (Array (FrontMatterS))
generatePostsHTML = do
  config <- askConfig
  cacheData <- readCacheData
  template <- readPostTemplate
  mdFiles <- readdir config.contentFolder >>= (\filename -> pure $ filter (contains (Pattern ".md")) filename)
  -- TODO: this is where we filter mdFiles to just the ones that need to be rebuilt because they have changed.
  postsMetadata <- parTraverse (\f -> generatePostHTML (Template template) cacheData f) mdFiles
  pure $ sortPosts $ filter (\d -> d.status == Published) postsMetadata

generatePostHTML :: Template -> CacheData -> String -> Aff (FrontMatterS)
generatePostHTML template cache fileName = do
  config <- askConfig
  fd <- md2FormattedData <$> readTextFile UTF8 (config.contentFolder <> "/" <> fileName)
  needsBuilding <- needsInvalidation' cache fileName
  when needsBuilding $ do
    case fd.frontMatter.status of
      Draft -> pure unit
      InvalidStatus s -> do
        throwError $ error $ "Invalid status '" <> s <> "' in '" <> fileName <> "'."
      Published -> do
        res <- try $ writeTextFile UTF8 (tmpFolder <> "/" <> fd.frontMatter.slug <> ".html") (replaceContentInTemplate template fd)
        case res of
          Left err -> Logs.logError $ "Could not write " <> fileName <> " to html (" <> show err <> ")"
          Right _ -> Logs.logSuccess $ "Wrote: " <> config.contentFolder <> "/" <> fileName <> " -> " <> tmpFolder <> "/" <> fd.frontMatter.slug <> ".html"
  pure fd.frontMatter

replaceContentInTemplate :: Template -> FormattedMarkdownData -> String
replaceContentInTemplate (Template template) pd =
  replaceAll (Pattern "{{title}}") (Replacement $ "<a href=\"./" <> pd.frontMatter.slug <> "\">" <> pd.frontMatter.title <> "</a>") template
    # replaceAll (Pattern "{{content}}") (Replacement pd.content)
    # replaceAll (Pattern "{{date}}") (Replacement $ formatDate "MMM DD, YYYY" pd.frontMatter.date)
    # replaceAll (Pattern "{{page_title}}") (Replacement pd.frontMatter.title)

readPostTemplate :: Aff String
readPostTemplate = do
  config <- askConfig
  readTextFile UTF8 config.newPostTemplate

generateStyles :: Aff Buffer
generateStyles = do
  config <- askConfig
  liftEffect
    $ do
        _ <- execSync (copyStyleFileToTmp config) defaultExecSyncOptions
        _ <- execSync installTailwindDeps options
        execSync command options
  where
  options = defaultExecSyncOptions { cwd = Just tmpFolder }

  copyStyleFileToTmp config = "cp " <> config.templateFolder <> "/style.css " <> tmpFolder <> "/style1.css"

  installTailwindDeps = "npm install tailwindcss"

  command = "npx @tailwindcss/cli -i style1.css -o style.css && rm style1.css"

recentPosts :: Int -> Array FrontMatterS -> String
recentPosts n xs =
  let
    recentN = take n xs
  in
    case recentN of
      [] -> "Nothing here."
      ys -> renderRecents ys
        where
        renderRecents fds = "<ul>" <> foldl fn "" fds <> "</ul>"

        fn b a = b <> "<li><a href=\"/" <> a.slug <> "\">" <> a.title <> "</a> &mdash; <span class=\"date\">" <> formatDate "MMM DD, YYYY" a.date <> "</span>" <> "</li>"

createHomePage :: Array FrontMatterS -> Aff Unit
createHomePage sortedArrayofPosts = do
  config <- askConfig
  recentsString <- pure $ recentPosts config.totalRecentPosts sortedArrayofPosts
  template <- readTextFile UTF8 (homepageTemplate config.templateFolder)
  categories <- pure $ (getCategoriesJson config.contentFolder # convertCategoriesToString)
  contents <-
    pure
      $ replaceAll (Pattern "{{recent_posts}}") (Replacement recentsString) template
          # replaceAll (Pattern "{{posts_by_categories}}") (Replacement categories)
  writeTextFile UTF8 (tmpFolder <> "/index.html") contents
  where
  convertCategoriesToString :: Array U.Category -> String
  convertCategoriesToString = foldl fn ""

  fn b a = b <> "<section><h3 class=\"category\">" <> a.category <> "</h3>" <> "<ul>" <> renderPosts a.posts <> "</ul></section>"

  renderPosts :: Array String -> String
  renderPosts posts = foldl fn2 "" (filteredPosts posts)

  filteredPosts :: Array String -> Array FrontMatterS
  filteredPosts xs =
    map
      ( \x ->
          find (\p -> p.slug == x) sortedArrayofPosts
      )
      xs
      # catMaybes
      # sortPosts

  fn2 b a = b <> "<li><a href=\"./" <> a.slug <> "\">" <> a.title <> "</a> &mdash; <span class=\"date\">" <> formatDate "MMM DD, YYYY" a.date <> "</span></li>"

-- getPostsAndSort :: Aff ({ postsToPublish :: Array String, postsToRebuild :: Array String })
-- getPostsAndSort = do
--   config <- askConfig
--   filePaths <- readdir config.contentFolder
--   onlyMarkdownFiles <- pure $ filter (contains (Pattern ".md")) filePaths
--   oldCacheData <- Cache.readCacheData
--   newCacheData <- Cache.createCacheData
--   formattedDataArray <- filePathsToProcessedData config onlyMarkdownFiles
--   removeDraft <- pure $ filter (\f -> f.frontMatter.status /= Draft) formattedDataArray
--   removeCached <- pure $ filter (\f -> Cache.needsInvalidation oldCacheData newCacheData f.frontMatter.slug) removeDraft
--   pure $ { postsToPublish: sortPosts removeDraft, postsToRebuild: sortPosts removeCached }
--   where
--   filePathsToProcessedData :: Config -> Array String -> Aff (Array FormattedMarkdownData)
--   filePathsToProcessedData config fpaths = parTraverse (\f -> readFileToData $ config.contentFolder <> "/" <> f) fpaths

sortPosts :: Array FrontMatterS -> Array FrontMatterS
sortPosts = sortBy (\a b -> if a.date < b.date then GT else LT)

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
      # Maybe.map (fromString)
      # join

groupedPostsToHTML :: Map Int (Array FrontMatterS) -> String
groupedPostsToHTML groupedPosts =
  let
    formattedDataToHTML :: FrontMatterS -> String
    formattedDataToHTML fd = "<li><a href=\"/" <> fd.slug <> "\">" <> fd.title <> "</a> &mdash; <span class=\"date\">" <> formatDate "MMM DD, YYYY" fd.date <> "</span></li>"

    arrayDataToHTML :: Array FrontMatterS -> String
    arrayDataToHTML fs = "<ul>" <> (map formattedDataToHTML fs # joinWith "") <> "</ul>"

    mapAsList :: Array (Tuple Int (Array FrontMatterS))
    mapAsList = Map.toUnfoldable groupedPosts # sortBy (\(Tuple a1 _) (Tuple a2 _) -> if a1 > a2 then LT else GT)

    tupleToString :: Tuple Int (Array FrontMatterS) -> String
    tupleToString (Tuple year fds) = "<section><h3>" <> show year <> "</h3><div>" <> arrayDataToHTML fds <> "</div></section>"

    result :: String
    result = map tupleToString mapAsList # joinWith ""
  in
    result

writeArchiveByYearPage :: Array (FrontMatterS) -> Aff Unit
writeArchiveByYearPage fds = do
  config <- askConfig
  contentToWrite <- pure $ groupedPostsToHTML $ groupPostsByYear fds
  templateContents <- readTextFile UTF8 $ archiveTemplate config.templateFolder
  replacedContent <- pure $ replaceAll (Pattern "{{content}}") (Replacement contentToWrite) templateContents
  writeTextFile UTF8 (tmpFolder <> "/archive.html") replacedContent

createNewPost :: Config -> String -> ExceptT Error Aff Unit
createNewPost config slug =
  ExceptT $ try
    $ do
        newPostTemplateContents <- readTextFile UTF8 (newPostTemplate config.templateFolder)
        today <- pure $ formatDate "YYYY-MM-DD" ""
        replaced <-
          pure
            $ replaceAll (Pattern "$date") (Replacement today) newPostTemplateContents
                # replaceAll (Pattern "$slug") (Replacement slug)
        writeTextFile UTF8 (config.contentFolder <> "/" <> slug <> ".md") replaced

cleanupNodeModules :: Aff Buffer
cleanupNodeModules = liftEffect $ execSync "rm -rf node_modules package-lock.json package.json" options
  where
  options = defaultExecSyncOptions { cwd = Just tmpFolder }
