module Main where

import Prelude

import Cache (CacheData, needsInvalidation, readCacheData)
import Cache as Cache
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (ask, lift)
import Control.Parallel (parTraverse, parTraverse_)
import Data.Array (catMaybes, drop, filter, find, foldl, head, sortBy, take)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (Pattern(..), Replacement(..), contains, joinWith, replaceAll, split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Effect.Exception (Error, error)
import Logs as Logs
import Node.Buffer (Buffer)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, writeTextFile)
import Node.Process (argv, exit)
import Prelude as Maybe
import RssGenerator as Rss
import Templates (archiveHtmlTemplate, feedTemplate, indexHtmlTemplate, notFoundTemplate, postHtmlTemplate, postMdTemplate, styleTemplate)
import Types (AppM, Category, Command(..), Config, FormattedMarkdownData, Status(..), Template(..), FrontMatterS)
import Utils (archiveTemplate, createFolderIfNotPresent, formatDate, getCategoriesJson, getConfig, homepageTemplate, liftAppM, md2FormattedData, newPostTemplate, runAppM, tmpFolder)

main :: Effect Unit
main = do
  args <- argv
  cmd <- pure $ mkCommand args
  case cmd of
    Test -> test
    Help -> log $ helpText
    ShowVersion -> log $ "v0.8.5"
    Init -> launchAff_ $ do
      config <- getConfig
      res <- runAppM config initApp
      case res of
        Left err -> Logs.logError $ "Could not initialize the app: " <> show err
        Right _ -> Logs.logSuccess $ "Templates generated in " <> config.templateFolder <> ". You can edit them."
    NewPost slug -> launchAff_ $ do
      config <- getConfig
      res <- runAppM config (createNewPost slug)
      case res of
        Left err -> do
          _ <- Logs.logError ("Could not create a new post: " <> show err)
          liftEffect $ exit 1
        Right _ -> Logs.logSuccess "Created new post. Happy writing!"
    Invalid -> do
      Logs.logError $ "Invalid command."
      log $ helpText
      exit 1
    Build -> launchAff_ $ do
      config <- getConfig
      res <- runAppM config buildSite
      _ <- liftEffect $ try $ execSync ("rm -rf " <> tmpFolder) defaultExecSyncOptions
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
  init - initialize the project.
  build - build the site.
  new [slug] - create a new post.
"""

buildSite :: AppM Unit
buildSite = do
  config <- ask
  liftAppM $ Logs.logInfo "Starting..."
  liftAppM $ createFolderIfNotPresent tmpFolder
  { published, draft } <- generatePostsHTML
  liftAppM $ Logs.logSuccess $ "Posts page generated."
  liftAppM $ Logs.logInfo $ "Generating archive page..."
  _ <- writeArchiveByYearPage published
  liftAppM $ Logs.logSuccess $ "Archive page generated."
  liftAppM $ Logs.logInfo $ "Generating home page..."
  _ <- createHomePage published
  liftAppM $ Logs.logSuccess $ "Home page generated."
  liftAppM $ Logs.logInfo $ "Copying 404.html..."
  _ <- liftEffect $ execSync ("cp " <> config.templateFolder <> "/404.html " <> tmpFolder) defaultExecSyncOptions
  liftAppM $ Logs.logSuccess $ "404.html copied."
  liftAppM $ Logs.logInfo "Copying images folder..."
  _ <- liftEffect $ execSync ("cp -r " <> "./images " <> tmpFolder) defaultExecSyncOptions
  liftAppM $ Logs.logSuccess $ "images folder copied."
  liftAppM $ Logs.logInfo "Copying js folder..."
  _ <- liftAppM $ liftEffect $ execSync ("cp -r " <> "./js " <> tmpFolder) defaultExecSyncOptions
  liftAppM $ Logs.logSuccess "js folder copied."
  liftAppM $ Logs.logInfo "Generating styles.css..."
  _ <- generateStyles
  liftAppM $ Logs.logSuccess "styles.css generated."
  liftAppM $ Logs.logInfo "Generating RSS feed..."
  _ <- Rss.generateRSSFeed (take 10 published)
  liftAppM $ Logs.logSuccess "RSS feed generated."
  liftAppM $ Logs.logInfo $ "Copying " <> tmpFolder <> " to " <> config.outputFolder
  _ <- removeDraftsFromOutput draft
  _ <- liftAppM $ createFolderIfNotPresent config.outputFolder
  _ <- liftAppM $ liftEffect $ execSync ("cp -r " <> tmpFolder <> "/* " <> config.outputFolder) defaultExecSyncOptions
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

generatePostsHTML :: AppM ({ published :: Array (FrontMatterS), draft :: Array (FrontMatterS), unlisted :: Array (FrontMatterS) })
generatePostsHTML = do
  config <- ask
  template <- readPostTemplate
  liftAppM $ do
    cacheData <- readCacheData
    mdFiles <- readdir config.contentFolder >>= (\filename -> pure $ filter (contains (Pattern ".md")) filename)
    postsMetadata <- parTraverse (\f -> generatePostHTML config (Template template) cacheData f) mdFiles
    pure $ { published: sortPosts $ filter (\d -> d.status == Published) postsMetadata, draft: sortPosts $ filter (\d -> d.status == Draft) postsMetadata, unlisted: sortPosts $ filter (\d -> d.status == Unlisted) postsMetadata }

generatePostHTML :: Config -> Template -> CacheData -> String -> Aff (FrontMatterS)
generatePostHTML config template cache fileName = do
  fd <- md2FormattedData <$> readTextFile UTF8 (config.contentFolder <> "/" <> fileName)
  needsBuilding <- needsInvalidation cache fileName
  when needsBuilding $ do
    case fd.frontMatter.status of
      Draft -> pure unit
      InvalidStatus s -> do
        throwError $ error $ "Invalid status '" <> s <> "' in '" <> fileName <> "'. Status can be 'draft', 'published' or 'unlisted'."
      Unlisted -> do
        res <- try $ writeTextFile UTF8 (tmpFolder <> "/" <> fd.frontMatter.slug <> ".html") (replaceContentInTemplate template fd)
        case res of
          Left err -> Logs.logError $ "Could not write " <> fileName <> " to html (" <> show err <> ")"
          Right _ -> Logs.logSuccess $ "Wrote: " <> config.contentFolder <> "/" <> fileName <> " -> " <> tmpFolder <> "/" <> fd.frontMatter.slug <> ".html"
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

readPostTemplate :: AppM String
readPostTemplate = do
  config <- ask
  liftAppM $ readTextFile UTF8 config.blogPostTemplate

generateStyles :: AppM Buffer
generateStyles = do
  config <- ask
  liftAppM $ liftEffect $ do
    _ <- execSync (copyStyleFileToTmp config) defaultExecSyncOptions
    execSync command options
  where
  options = defaultExecSyncOptions { cwd = Just tmpFolder }
  copyStyleFileToTmp config = "cp " <> config.templateFolder <> "/style.css " <> tmpFolder <> "/style1.css"
  command = "tailwindcss -i style1.css -o style.css --minify && rm style1.css"

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

createHomePage :: Array FrontMatterS -> AppM Unit
createHomePage sortedArrayofPosts = do
  config <- ask
  liftAppM $ do
    recentsString <- pure $ recentPosts config.totalRecentPosts sortedArrayofPosts
    template <- readTextFile UTF8 (homepageTemplate config.templateFolder)
    categories <- pure $ (getCategoriesJson config.contentFolder # convertCategoriesToString)
    contents <-
      pure
        $ replaceAll (Pattern "{{recent_posts}}") (Replacement recentsString) template
            # replaceAll (Pattern "{{posts_by_categories}}") (Replacement categories)
    writeTextFile UTF8 (tmpFolder <> "/index.html") contents
  where
  convertCategoriesToString :: Array Category -> String
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

writeArchiveByYearPage :: Array (FrontMatterS) -> AppM Unit
writeArchiveByYearPage fds = do
  config <- ask
  liftAppM $ do
    contentToWrite <- pure $ groupedPostsToHTML $ groupPostsByYear fds
    templateContents <- readTextFile UTF8 $ archiveTemplate config.templateFolder
    replacedContent <- pure $ replaceAll (Pattern "{{content}}") (Replacement contentToWrite) templateContents
    writeTextFile UTF8 (tmpFolder <> "/archive.html") replacedContent

createNewPost :: String -> AppM Unit
createNewPost slug = do
  config <- ask
  liftAppM $ do
    newPostTemplateContents <- readTextFile UTF8 (newPostTemplate config.templateFolder)
    today <- pure $ formatDate "YYYY-MM-DD" ""
    replaced <-
      pure
        $ replaceAll (Pattern "$date") (Replacement today) newPostTemplateContents
            # replaceAll (Pattern "$slug") (Replacement slug)
    writeTextFile UTF8 (config.contentFolder <> "/" <> slug <> ".md") replaced

initApp :: AppM Unit
initApp = do
  config <- ask
  liftAppM $ do
    createFolderIfNotPresent config.templateFolder
    createFolderIfNotPresent config.contentFolder
    createFolderIfNotPresent "images"
    createFolderIfNotPresent "js"
    Logs.logInfo "Generating index.html..."
    writeTextFile UTF8 (config.templateFolder <> "/index.html") indexHtmlTemplate
    Logs.logInfo "Generating post.html..."
    writeTextFile UTF8 (config.templateFolder <> "/post.html") postHtmlTemplate
    Logs.logInfo "Generating style.css..."
    writeTextFile UTF8 (config.templateFolder <> "/style.css") styleTemplate
    Logs.logInfo "Generating feed.xml..."
    when (isNothing config.domain) $ Logs.logWarning "feed.xml template is missing domain because you have not set SITE_URL in the environment. Manually edit the feed.xml file to add the correct domain. When building the site, you will need to set the domain in your shell enviroment. (e.g SITE_URL=https://my.blog)"
    writeTextFile UTF8 (config.templateFolder <> "/feed.xml") (feedTemplate (fromMaybe "https://my.blog" config.domain))
    Logs.logInfo "Generating archive.html..."
    writeTextFile UTF8 (config.templateFolder <> "/archive.html") archiveHtmlTemplate
    Logs.logInfo "Generating 404.html..."
    writeTextFile UTF8 (config.templateFolder <> "/404.html") notFoundTemplate
    Logs.logInfo "Generating new post markdown template..."
    writeTextFile UTF8 (config.templateFolder <> "/post.md") postMdTemplate
    Logs.logSuccess "Done! You can now edit these templates. Just retain the handlebars."

removeDraftsFromOutput :: Array FrontMatterS -> AppM Unit
removeDraftsFromOutput drafts = do
  config <- ask
  liftAppM $ parTraverse_ (removeDraftHtml config.outputFolder) drafts
  where
  removeDraftHtml :: String -> FrontMatterS -> Aff Unit
  removeDraftHtml folder draftItem = do
    _ <- try $ liftEffect $ execSync ("rm " <> folder <> "/" <> draftItem.slug <> ".html") defaultExecSyncOptions
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