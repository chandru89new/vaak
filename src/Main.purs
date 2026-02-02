module Main where

import Prelude

import Cache (CacheData, needsInvalidation, readCacheData)
import Cache as Cache
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (ask, lift)
import Control.Parallel (parTraverse, parTraverse_)
import Data.Array (drop, filter, head, sortBy, take)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (Pattern(..), Replacement(..), contains, replaceAll)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Effect.Exception (Error, error)
import Logs as Logs
import Node.Buffer (Buffer)
import Node.ChildProcess (execSync, execSync')
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, writeTextFile)
import Node.FS.Sync (exists)
import Node.Process (argv, exit')
import Nunjucks (renderTemplate)
import RssGenerator as Rss
import Templates (archiveHtmlTemplate, feedTemplate, indexHtmlTemplate, notFoundHtmlTemplate, postHtmlTemplate, postMdTemplate, styleTemplate)
import Types (AppM, Command(..), Config, Status(..), FrontMatterS)
import Utils (createFolderIfNotPresent, defaultExecSyncOptions, fileNameExists, folderExists, formatDate, getConfig, groupPostsByYearArray, liftAppM, md2FormattedData, prepare404Context, prepareArchiveContext, prepareIndexContext, preparePostContext, runAppM, templateFolder, tmpFolder)

main :: Effect Unit
main = do
  args <- argv
  cmd <- pure $ mkCommand args
  case cmd of
    Test -> test
    Help -> log $ helpText
    ShowVersion -> log $ "v0.9.3"
    Init -> launchAff_ $ do
      config <- getConfig
      res <- runAppM config initApp
      case res of
        Left err -> Logs.logError $ "Could not initialize the app: " <> show err
        Right _ -> Logs.logSuccess $ "Templates generated in " <> templateFolder <> ". You can edit them."
    NewPost slug -> launchAff_ $ do
      config <- getConfig
      res <- runAppM config (createNewPost slug)
      case res of
        Left err -> do
          _ <- Logs.logError ("Could not create a new post: " <> show err)
          liftEffect $ exit' 1
        Right _ -> Logs.logSuccess "Created new post. Happy writing!"
    Invalid -> do
      Logs.logError $ "Invalid command."
      log $ helpText
      exit' 1
    Build -> launchAff_ $ do
      config <- getConfig
      res <- runAppM config buildSite
      _ <- liftEffect $ try $ execSync ("rm -rf " <> tmpFolder)
      case res of
        Left err -> do
          Logs.logError $ "Error when building the site: " <> show err
          liftEffect $ exit' 1
        Right _ -> Logs.logSuccess $ "Site built and available in the " <> config.outputFolder <> " folder."

helpText :: String
helpText =
  """Commands:
  help - print this help text.
  version - print version info.
  init - initialize the project.
  build - build the site.
  new [slug] - create a new post.
"""

buildSite :: AppM Unit
buildSite = do
  config <- ask
  when (isNothing config.domain) $ liftAppM $ throwError $ error "SITE_URL is required. Set it in your environment (e.g. SITE_URL=https://my.blog)."
  when (isNothing config.siteName) $ liftAppM $ throwError $ error "SITE_NAME is required. Set it in your environment (e.g. SITE_NAME=\"My Blog\")."
  liftAppM $ Logs.logInfo "Starting..."
  liftAppM $ createFolderIfNotPresent tmpFolder
  { published, draft } <- generatePostsHTML
  liftAppM $ Logs.logSuccess "Posts generated."
  liftAppM $ Logs.logInfo "Generating home page..."
  _ <- createHomePage published
  liftAppM $ Logs.logSuccess "Home page generated."
  liftAppM $ Logs.logInfo "Generating archive page..."
  _ <- writeArchiveByYearPage published
  liftAppM $ Logs.logSuccess "Archive page generated."
  liftAppM $ Logs.logInfo "Generating RSS feed..."
  _ <- Rss.generateRSSFeed (take 10 published)
  liftAppM $ Logs.logSuccess "RSS feed generated."
  liftAppM $ Logs.logInfo "Generating 404 page..."
  _ <- write404Page
  liftAppM $ Logs.logSuccess "404 page generated."
  liftAppM $ Logs.logInfo "Copying images folder..."
  _ <- liftEffect $ execSync ("cp -r " <> "./images " <> tmpFolder)
  liftAppM $ Logs.logSuccess "images folder copied."
  liftAppM $ Logs.logInfo "Copying js folder..."
  _ <- liftAppM $ liftEffect $ execSync ("cp -r " <> "./js " <> tmpFolder)
  liftAppM $ Logs.logSuccess "js folder copied."
  liftAppM $ Logs.logInfo "Generating styles.css..."
  _ <- generateStyles
  liftAppM $ Logs.logSuccess "styles.css generated."
  _ <- removeDraftsFromOutput draft
  liftAppM $ Logs.logInfo $ "Copying " <> tmpFolder <> " to " <> config.outputFolder
  _ <- liftAppM $ createFolderIfNotPresent config.outputFolder
  _ <- liftAppM $ liftEffect $ execSync ("cp -r " <> tmpFolder <> "/* " <> config.outputFolder)
  liftAppM $ Logs.logSuccess "Copied."
  liftAppM $ Logs.logInfo "Updating cache..."
  _ <- Cache.writeCacheData
  liftAppM $ Logs.logSuccess "Cached updated."

mkCommand :: Array String -> Command
mkCommand xs = case head (drop 2 xs) of
  Just "test" -> Test
  Just "version" -> ShowVersion
  Just "help" -> Help
  Just "init" -> Init
  Just "build" -> Build
  Just "new" -> case head $ drop 3 xs of
    Just slug -> NewPost slug
    _ -> Invalid
  _ -> Invalid

generatePostsHTML :: AppM { published :: Array FrontMatterS, draft :: Array FrontMatterS, unlisted :: Array FrontMatterS }
generatePostsHTML = do
  config <- ask
  liftAppM $ do
    cacheData <- readCacheData
    mdFiles <- readdir config.contentFolder >>= (\filename -> pure $ filter (contains (Pattern ".md")) filename)
    postsMetadata <- parTraverse (\f -> generatePostHTML config cacheData f) mdFiles
    pure $ { published: sortPosts $ filter (\d -> d.status == Published) postsMetadata, draft: sortPosts $ filter (\d -> d.status == Draft) postsMetadata, unlisted: sortPosts $ filter (\d -> d.status == Unlisted) postsMetadata }

generatePostHTML :: Config -> CacheData -> String -> Aff FrontMatterS
generatePostHTML config cache fileName = do
  fd <- md2FormattedData <$> readTextFile UTF8 (config.contentFolder <> "/" <> fileName)
  needsBuilding <- needsInvalidation cache fileName
  when needsBuilding $ do
    case fd.frontMatter.status of
      Draft -> pure unit
      InvalidStatus s -> do
        throwError $ error $ "Invalid status '" <> s <> "' in '" <> fileName <> "'. Status can be 'draft', 'published' or 'unlisted'."
      Unlisted -> writePost fd
      Published -> writePost fd
  pure fd.frontMatter
  where
  writePost fd = do
    let context = preparePostContext config fd.frontMatter fd.content
    html <- renderTemplate (templateFolder <> "/post.html") context
    res <- try $ writeTextFile UTF8 (tmpFolder <> "/" <> fd.frontMatter.slug <> ".html") html
    case res of
      Left err -> Logs.logError $ "Could not write " <> fileName <> " to html (" <> show err <> ")"
      Right _ -> Logs.logSuccess $ "Wrote: " <> config.contentFolder <> "/" <> fileName <> " -> " <> tmpFolder <> "/" <> fd.frontMatter.slug <> ".html"

generateStyles :: AppM Buffer
generateStyles = do
  liftAppM $ liftEffect $ do
    _ <- execSync copyStyleFileToTmp
    execSync' command (\_ -> options)
  where
  options = defaultExecSyncOptions { cwd = Just tmpFolder }
  copyStyleFileToTmp = "cp " <> templateFolder <> "/style.css " <> tmpFolder <> "/style1.css"
  command = "tailwindcss -i style1.css -o style.css --minify && rm style1.css"

createHomePage :: Array FrontMatterS -> AppM Unit
createHomePage sortedArrayofPosts = do
  config <- ask
  liftAppM $ do
    let context = prepareIndexContext config sortedArrayofPosts
    html <- renderTemplate (templateFolder <> "/index.html") context
    writeTextFile UTF8 (tmpFolder <> "/index.html") html

sortPosts :: Array FrontMatterS -> Array FrontMatterS
sortPosts = sortBy (\a b -> if a.date < b.date then GT else LT)

writeArchiveByYearPage :: Array FrontMatterS -> AppM Unit
writeArchiveByYearPage fds = do
  config <- ask
  liftAppM $ do
    let groupedPosts = groupPostsByYearArray fds
    let context = prepareArchiveContext config groupedPosts
    html <- renderTemplate (templateFolder <> "/archive.html") context
    writeTextFile UTF8 (tmpFolder <> "/archive.html") html

write404Page :: AppM Unit
write404Page = do
  config <- ask
  liftAppM $ do
    let context = prepare404Context config
    html <- renderTemplate (templateFolder <> "/404.html") context
    writeTextFile UTF8 (tmpFolder <> "/404.html") html

createNewPost :: String -> AppM Unit
createNewPost slug = do
  when (contains (Pattern "/") slug || contains (Pattern "\\") slug) $ throwError $ error "Slug cannot contain '/' or '\\' characters."
  config <- ask
  fileExists <- fileNameExists slug config.contentFolder
  liftAppM $ do
    when fileExists $ throwError $ error $ "Cannot create a new file because there's one already with the same name."
    newPostTemplateContents <- readTextFile UTF8 (templateFolder <> "/post.md")
    today <- pure $ formatDate "YYYY-MM-DD" ""
    replaced <-
      pure
        $ replaceAll (Pattern "$date") (Replacement today) newPostTemplateContents
            # replaceAll (Pattern "$slug") (Replacement slug)
    writeTextFile UTF8 (config.contentFolder <> "/" <> slug <> ".md") replaced

initApp :: AppM Unit
initApp = do
  config <- ask
  alreadyInited <- folderExists templateFolder
  when alreadyInited $ do
    throwError $ error $ "Looks like you've already initialized the project. Please remove the '" <> templateFolder <> "' folder and try again."
  liftAppM $ do
    createFolderIfNotPresent templateFolder
    createFolderIfNotPresent config.contentFolder
    createFolderIfNotPresent "images"
    createFolderIfNotPresent "js"
    Logs.logInfo "Generating index.html..."
    writeTextFile UTF8 (templateFolder <> "/index.html") indexHtmlTemplate
    Logs.logInfo "Generating post.html..."
    writeTextFile UTF8 (templateFolder <> "/post.html") postHtmlTemplate
    Logs.logInfo "Generating style.css..."
    writeTextFile UTF8 (templateFolder <> "/style.css") styleTemplate
    Logs.logInfo "Generating feed.xml..."
    when (isNothing config.domain) $ throwError $ error "SITE_URL is required. Set it in your environment (e.g. SITE_URL=https://my.blog)."
    when (isNothing config.siteName) $ throwError $ error "SITE_NAME is required. Set it in your environment (e.g. SITE_NAME=\"My Blog\")."
    writeTextFile UTF8 (templateFolder <> "/feed.xml") (feedTemplate (fromMaybe "" config.domain) (fromMaybe "" config.siteName))
    Logs.logInfo "Generating archive.html..."
    writeTextFile UTF8 (templateFolder <> "/archive.html") archiveHtmlTemplate
    Logs.logInfo "Generating 404.html..."
    writeTextFile UTF8 (templateFolder <> "/404.html") notFoundHtmlTemplate
    Logs.logInfo "Generating new post markdown template..."
    writeTextFile UTF8 (templateFolder <> "/post.md") postMdTemplate

removeDraftsFromOutput :: Array FrontMatterS -> AppM Unit
removeDraftsFromOutput drafts = do
  config <- ask
  liftAppM $ parTraverse_ (removeDraftHtml config.outputFolder) drafts
  where
  removeDraftHtml :: String -> FrontMatterS -> Aff Unit
  removeDraftHtml folder draftItem = do
    fileExists <- liftEffect $ exists ("" <> folder <> "/" <> draftItem.slug <> ".html")
    when fileExists $ do
      _ <- try $ liftEffect $ execSync ("rm " <> folder <> "/" <> draftItem.slug <> ".html")
      pure unit

test :: Effect Unit
test = launchAff_ $ do
  config <- getConfig
  res <- runAppM config $ do
    lift $ ExceptT $ do
      testAff
  liftEffect $ logShow res

testAff :: Aff (Either Error Unit)
testAff = do
  throwError $ error "test error"