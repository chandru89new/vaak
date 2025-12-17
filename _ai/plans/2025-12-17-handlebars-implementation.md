# Handlebars Templating Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace hard-coded HTML generation with Handlebars templating, allowing users to fully customize output HTML structure.

**Architecture:** Add Handlebars FFI module, refactor build flow to prepare data → compile template → render → write. Remove all HTML-generating functions from Main.purs.

**Tech Stack:** PureScript, Handlebars (npm), esbuild bundling

---

## Task 1: Add Handlebars Dependency

**Files:**
- Modify: `package.json`

**Step 1: Add handlebars to dependencies**

In `package.json`, add handlebars to dependencies:

```json
{
  "dependencies": {
    "dayjs": "^1.11.7",
    "esbuild": "^0.24.2",
    "gray-matter": "^4.0.3",
    "handlebars": "^4.7.8",
    "js-yaml": "^4.1.0",
    ...
  }
}
```

**Step 2: Install dependencies**

Run: `yarn install`
Expected: handlebars added to node_modules

**Step 3: Commit**

```bash
git add package.json yarn.lock
git commit -m "feat: add handlebars dependency"
```

---

## Task 2: Create Handlebars FFI Module

**Files:**
- Create: `src/Handlebars.js`
- Create: `src/Handlebars.purs`

**Step 1: Create JavaScript FFI implementation**

Create `src/Handlebars.js`:

```javascript
import Handlebars from "handlebars";

// compileTemplate :: String -> (Foreign -> String)
const compileTemplate = (templateString) => {
  const compiled = Handlebars.compile(templateString);
  return (context) => compiled(context);
};

// renderTemplate :: (Foreign -> String) -> Foreign -> String
const renderTemplate = (compiledFn) => (context) => compiledFn(context);

export { compileTemplate, renderTemplate };
```

**Step 2: Create PureScript FFI declarations**

Create `src/Handlebars.purs`:

```purescript
module Handlebars where

import Foreign (Foreign)

-- A compiled Handlebars template function
foreign import data CompiledTemplate :: Type

-- Compile a template string into a reusable template function
foreign import compileTemplate :: String -> CompiledTemplate

-- Render a compiled template with a context object
foreign import renderTemplate :: CompiledTemplate -> Foreign -> String
```

**Step 3: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add src/Handlebars.js src/Handlebars.purs
git commit -m "feat: add Handlebars FFI module"
```

---

## Task 3: Update Templates.purs with .hbs Templates

**Files:**
- Modify: `src/Templates.purs`

**Step 1: Rename HTML templates to .hbs and update content**

Replace `indexHtmlTemplate` with `indexHbsTemplate`:

```purescript
indexHbsTemplate :: String
indexHbsTemplate =
  """<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>my blog</title>
  <link href="./style.css" rel="stylesheet" />
  <link href="./images/favicon.png" rel="icon" />
</head>

<body>
  <div>
    <h3 id="home-page-logo">
      <a href="/">
        my blog
      </a>
    </h3>
    <header>
      <p>this is my blog. there may be many other blogs. but this is my blog.</p>
    </header>
    <article id="archive_container">
      <section>
        <h3>Most recently</h3>
        <div>
          <ul>
            {{#each allPosts}}
            <li><a href="./{{slug}}">{{title}}</a> &mdash; <span class="date">{{date}}</span></li>
            {{/each}}
          </ul>
        </div>
      </section>
      <section>
        <h3>Archives</h3>
        <div>
          <a href="./archive">See the full archive &rarr;</a>
        </div>
      </section>
    </article>
    <footer>
      <a href="/archive">archive</a>
      <span>&bull;</span>
      <a href="/feed.xml">rss feed</a>
    </footer>
  </div>
</body>

</html>"""
```

**Step 2: Replace postHtmlTemplate with postHbsTemplate**

```purescript
postHbsTemplate :: String
postHbsTemplate =
  """<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{{title}} — my blog</title>
  <link href="./style.css" rel="stylesheet" />
  <link href="./images/favicon.png" rel="icon" />
</head>

<body>
  <div>
    <h3 id="logo">
      <a href="/">
        &larr; my blog
      </a>
    </h3>
    <header>
      <h1>{{title}}</h1>
      <div class="date">{{date}}</div>
    </header>
    <article>{{{content}}}</article>
    <footer>
      <a href="/">&larr; blog</a>
      <span>&bull;</span>
      <a href="/archive">archives</a>
      <span>&bull;</span>
      <a href="/feed.xml">rss feed</a>
    </footer>
  </div>
  <script>
    document.addEventListener("DOMContentLoaded", function () {
      const articleLinks = Array.from(document.querySelectorAll("article a"));
      const externalArticleLinks = articleLinks.filter(
        (link) => link?.getAttribute("href")?.startsWith("http")
      );
      externalArticleLinks.forEach((link) => link?.setAttribute("target", "_blank"));
    });
  </script>
</body>

</html>"""
```

**Step 3: Replace archiveHtmlTemplate with archiveHbsTemplate**

```purescript
archiveHbsTemplate :: String
archiveHbsTemplate = """<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Archives — my blog</title>
  <link href="./style.css" rel="stylesheet" />
  <link href="./images/favicon.png" rel="icon" />
</head>

<body>
  <div>
    <h3 id="logo">
      <a href="/">
        &larr; my blog
      </a>
    </h3>
    <header>
      <h1>Archive</h1>
    </header>
    <article id="archive_container">
      {{#each postsByYear}}
      <section>
        <h3>{{year}}</h3>
        <ul>
          {{#each posts}}
          <li><a href="/{{slug}}">{{title}}</a> &mdash; <span class="date">{{date}}</span></li>
          {{/each}}
        </ul>
      </section>
      {{/each}}
    </article>
    <footer>
      <a href="/">&larr; blog</a>
      <span>&bull;</span>
      <a href="/feed.xml">rss feed</a>
    </footer>
  </div>
</body>

</html>"""
```

**Step 4: Replace notFoundTemplate with notFoundHbsTemplate**

```purescript
notFoundHbsTemplate :: String
notFoundHbsTemplate = """<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Not Found — my blog</title>
  <link href="./style.css" rel="stylesheet" />
  <link href="./images/favicon.png" rel="icon" />
</head>

<body>
  <div>
    <h3 id="home-page-logo">
      <a href="/">
        my blog
      </a>
    </h3>
    <header>
    </header>
    <article>
      <section>
        <p>Uh, that page doesn't exist. Sorry about that.</p>
      </section>
      <div>
        <a href="./archive">See the full archive &rarr;</a>
      </div>
    </article>
    <footer>
      <a href="/">&larr; blog</a>
    </footer>
  </div>
</body>

</html>"""
```

**Step 5: Update exports**

Update the module exports to use new names:
- `indexHbsTemplate` (was `indexHtmlTemplate`)
- `postHbsTemplate` (was `postHtmlTemplate`)
- `archiveHbsTemplate` (was `archiveHtmlTemplate`)
- `notFoundHbsTemplate` (was `notFoundTemplate`)

Keep `feedTemplate`, `postMdTemplate`, `styleTemplate` unchanged.

**Step 6: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 7: Commit**

```bash
git add src/Templates.purs
git commit -m "feat: convert HTML templates to Handlebars format"
```

---

## Task 4: Update Utils.purs for .hbs File Paths

**Files:**
- Modify: `src/Utils.purs`

**Step 1: Update template file paths to use .hbs extension**

Change these functions:

```purescript
defaultBlogpostTemplate :: FilePath -> String
defaultBlogpostTemplate templateFolder = templateFolder <> "/post.hbs"

archiveTemplate :: FilePath -> String
archiveTemplate templateFolder = templateFolder <> "/archive.hbs"

homepageTemplate :: FilePath -> String
homepageTemplate templateFolder = templateFolder <> "/index.hbs"

notFoundTemplate :: FilePath -> String
notFoundTemplate templateFolder = templateFolder <> "/404.hbs"
```

**Step 2: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add src/Utils.purs
git commit -m "feat: update template paths to use .hbs extension"
```

---

## Task 5: Add Data Preparation Functions to Utils.js

**Files:**
- Modify: `src/Utils.js`

**Step 1: Add preparePostContext function**

Add to `src/Utils.js`:

```javascript
// Prepare context for post.hbs template
const preparePostContext = (formatFn) => (frontMatter) => (content) => (siteUrl) => ({
  title: frontMatter.title,
  date: formatFn("MMM DD, YYYY")(frontMatter.date),
  slug: frontMatter.slug,
  content: content,
  siteUrl: siteUrl || ""
});
```

**Step 2: Add prepareIndexContext function**

```javascript
// Prepare context for index.hbs template
const prepareIndexContext = (formatFn) => (allPosts) => (siteUrl) => ({
  allPosts: allPosts.map(post => ({
    title: post.title,
    date: formatFn("MMM DD, YYYY")(post.date),
    slug: post.slug
  })),
  siteUrl: siteUrl || ""
});
```

**Step 3: Add prepareArchiveContext function**

```javascript
// Prepare context for archive.hbs template
const prepareArchiveContext = (formatFn) => (groupedPosts) => (siteUrl) => ({
  postsByYear: groupedPosts.map(group => ({
    year: group.year,
    posts: group.posts.map(post => ({
      title: post.title,
      date: formatFn("MMM DD, YYYY")(post.date),
      slug: post.slug
    }))
  })),
  siteUrl: siteUrl || ""
});
```

**Step 4: Add prepare404Context function**

```javascript
// Prepare context for 404.hbs template
const prepare404Context = (siteUrl) => ({
  siteUrl: siteUrl || ""
});
```

**Step 5: Export new functions**

Update the export statement:

```javascript
export {
  md2RawFormattedData,
  formatDate,
  getCategoriesJson,
  preparePostContext,
  prepareIndexContext,
  prepareArchiveContext,
  prepare404Context
};
```

**Step 6: Commit**

```bash
git add src/Utils.js
git commit -m "feat: add data preparation functions for Handlebars contexts"
```

---

## Task 6: Add FFI Declarations for Data Preparation

**Files:**
- Modify: `src/Utils.purs`

**Step 1: Add Foreign import and type aliases**

Add imports at top of file:

```purescript
import Foreign (Foreign)
```

**Step 2: Add FFI declarations for context preparation**

Add these foreign imports:

```purescript
foreign import preparePostContext :: (String -> String -> String) -> FrontMatterS -> String -> String -> Foreign

foreign import prepareIndexContext :: (String -> String -> String) -> Array FrontMatterS -> String -> Foreign

foreign import prepareArchiveContext :: (String -> String -> String) -> Array { year :: Int, posts :: Array FrontMatterS } -> String -> Foreign

foreign import prepare404Context :: String -> Foreign
```

**Step 3: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add src/Utils.purs
git commit -m "feat: add FFI declarations for context preparation functions"
```

---

## Task 7: Refactor initApp to Generate .hbs Files

**Files:**
- Modify: `src/Main.purs`

**Step 1: Update imports**

Add/update imports in Main.purs:

```purescript
import Templates (archiveHbsTemplate, feedTemplate, indexHbsTemplate, notFoundHbsTemplate, postHbsTemplate, postMdTemplate, styleTemplate)
import Utils (archiveTemplate, createFolderIfNotPresent, formatDate, getConfig, homepageTemplate, liftAppM, md2FormattedData, newPostTemplate, notFoundTemplate, runAppM, tmpFolder)
```

**Step 2: Update initApp function**

Replace the template generation section in `initApp`:

```purescript
initApp :: AppM Unit
initApp = do
  config <- ask
  liftAppM $ do
    createFolderIfNotPresent config.templateFolder
    createFolderIfNotPresent config.contentFolder
    createFolderIfNotPresent "images"
    createFolderIfNotPresent "js"
    Logs.logInfo "Generating index.hbs..."
    writeTextFile UTF8 (config.templateFolder <> "/index.hbs") indexHbsTemplate
    Logs.logInfo "Generating post.hbs..."
    writeTextFile UTF8 (config.templateFolder <> "/post.hbs") postHbsTemplate
    Logs.logInfo "Generating style.css..."
    writeTextFile UTF8 (config.templateFolder <> "/style.css") styleTemplate
    Logs.logInfo "Generating feed.xml..."
    when (isNothing config.domain) $ Logs.logWarning "feed.xml template is missing domain because you have not set SITE_URL in the environment. Manually edit the feed.xml file to add the correct domain. When building the site, you will need to set the domain in your shell enviroment. (e.g SITE_URL=https://my.blog)"
    writeTextFile UTF8 (config.templateFolder <> "/feed.xml") (feedTemplate (fromMaybe "https://my.blog" config.domain))
    Logs.logInfo "Generating archive.hbs..."
    writeTextFile UTF8 (config.templateFolder <> "/archive.hbs") archiveHbsTemplate
    Logs.logInfo "Generating 404.hbs..."
    writeTextFile UTF8 (config.templateFolder <> "/404.hbs") notFoundHbsTemplate
    Logs.logInfo "Generating new post markdown template..."
    writeTextFile UTF8 (config.templateFolder <> "/post.md") postMdTemplate
    Logs.logSuccess "Done! You can now edit these templates."
```

**Step 3: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add src/Main.purs
git commit -m "feat: update initApp to generate .hbs template files"
```

---

## Task 8: Refactor buildSite to Use Handlebars

**Files:**
- Modify: `src/Main.purs`

**Step 1: Add Handlebars import**

Add to imports:

```purescript
import Handlebars (CompiledTemplate, compileTemplate, renderTemplate)
```

**Step 2: Add template loading function**

Add helper function to load and compile templates:

```purescript
loadTemplates :: AppM { index :: CompiledTemplate, post :: CompiledTemplate, archive :: CompiledTemplate, notFound :: CompiledTemplate }
loadTemplates = do
  config <- ask
  liftAppM $ do
    indexTpl <- readTextFile UTF8 (homepageTemplate config.templateFolder)
    postTpl <- readTextFile UTF8 (defaultBlogpostTemplate config.templateFolder)
    archiveTpl <- readTextFile UTF8 (archiveTemplate config.templateFolder)
    notFoundTpl <- readTextFile UTF8 (notFoundTemplate config.templateFolder)
    pure
      { index: compileTemplate indexTpl
      , post: compileTemplate postTpl
      , archive: compileTemplate archiveTpl
      , notFound: compileTemplate notFoundTpl
      }
```

Note: Add `defaultBlogpostTemplate` to Utils imports, and add `notFoundTemplate` helper in Utils.purs.

**Step 3: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add src/Main.purs
git commit -m "feat: add template loading function for Handlebars"
```

---

## Task 9: Refactor generatePostHTML to Use Handlebars

**Files:**
- Modify: `src/Main.purs`

**Step 1: Update generatePostHTML signature and implementation**

Replace `generatePostHTML` function:

```purescript
generatePostHTML :: Config -> CompiledTemplate -> CacheData -> String -> Aff FrontMatterS
generatePostHTML config template cache fileName = do
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
    let context = preparePostContext formatDate fd.frontMatter fd.content (fromMaybe "" config.domain)
    let html = renderTemplate template context
    res <- try $ writeTextFile UTF8 (tmpFolder <> "/" <> fd.frontMatter.slug <> ".html") html
    case res of
      Left err -> Logs.logError $ "Could not write " <> fileName <> " to html (" <> show err <> ")"
      Right _ -> Logs.logSuccess $ "Wrote: " <> config.contentFolder <> "/" <> fileName <> " -> " <> tmpFolder <> "/" <> fd.frontMatter.slug <> ".html"
```

**Step 2: Update imports**

Add to imports:

```purescript
import Utils (preparePostContext, ...)
```

**Step 3: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add src/Main.purs
git commit -m "feat: refactor generatePostHTML to use Handlebars"
```

---

## Task 10: Refactor generatePostsHTML to Pass Template

**Files:**
- Modify: `src/Main.purs`

**Step 1: Update generatePostsHTML to accept compiled template**

```purescript
generatePostsHTML :: CompiledTemplate -> AppM { published :: Array FrontMatterS, draft :: Array FrontMatterS, unlisted :: Array FrontMatterS }
generatePostsHTML postTemplate = do
  config <- ask
  liftAppM $ do
    cacheData <- readCacheData
    mdFiles <- readdir config.contentFolder >>= (\filename -> pure $ filter (contains (Pattern ".md")) filename)
    postsMetadata <- parTraverse (\f -> generatePostHTML config postTemplate cacheData f) mdFiles
    pure $ { published: sortPosts $ filter (\d -> d.status == Published) postsMetadata, draft: sortPosts $ filter (\d -> d.status == Draft) postsMetadata, unlisted: sortPosts $ filter (\d -> d.status == Unlisted) postsMetadata }
```

**Step 2: Remove readPostTemplate function**

Delete the `readPostTemplate` function (no longer needed).

**Step 3: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add src/Main.purs
git commit -m "feat: update generatePostsHTML to use compiled template"
```

---

## Task 11: Refactor createHomePage to Use Handlebars

**Files:**
- Modify: `src/Main.purs`

**Step 1: Replace createHomePage function**

```purescript
createHomePage :: CompiledTemplate -> Array FrontMatterS -> AppM Unit
createHomePage template sortedArrayofPosts = do
  config <- ask
  liftAppM $ do
    let context = prepareIndexContext formatDate sortedArrayofPosts (fromMaybe "" config.domain)
    let html = renderTemplate template context
    writeTextFile UTF8 (tmpFolder <> "/index.html") html
```

**Step 2: Update imports**

Add `prepareIndexContext` to Utils imports.

**Step 3: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add src/Main.purs
git commit -m "feat: refactor createHomePage to use Handlebars"
```

---

## Task 12: Refactor writeArchiveByYearPage to Use Handlebars

**Files:**
- Modify: `src/Main.purs`

**Step 1: Add groupPostsByYearArray helper**

Add helper to convert Map to Array format:

```purescript
groupPostsByYearArray :: Array FrontMatterS -> Array { year :: Int, posts :: Array FrontMatterS }
groupPostsByYearArray posts =
  let
    grouped = groupPostsByYear posts
    asList = Map.toUnfoldable grouped # sortBy (\(Tuple a1 _) (Tuple a2 _) -> if a1 > a2 then LT else GT)
  in
    map (\(Tuple year ps) -> { year, posts: ps }) asList
```

**Step 2: Replace writeArchiveByYearPage function**

```purescript
writeArchiveByYearPage :: CompiledTemplate -> Array FrontMatterS -> AppM Unit
writeArchiveByYearPage template fds = do
  config <- ask
  liftAppM $ do
    let groupedPosts = groupPostsByYearArray fds
    let context = prepareArchiveContext formatDate groupedPosts (fromMaybe "" config.domain)
    let html = renderTemplate template context
    writeTextFile UTF8 (tmpFolder <> "/archive.html") html
```

**Step 3: Update imports**

Add `prepareArchiveContext` to Utils imports.

**Step 4: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 5: Commit**

```bash
git add src/Main.purs
git commit -m "feat: refactor writeArchiveByYearPage to use Handlebars"
```

---

## Task 13: Add 404 Page Generation

**Files:**
- Modify: `src/Main.purs`

**Step 1: Add write404Page function**

```purescript
write404Page :: CompiledTemplate -> AppM Unit
write404Page template = do
  config <- ask
  liftAppM $ do
    let context = prepare404Context (fromMaybe "" config.domain)
    let html = renderTemplate template context
    writeTextFile UTF8 (tmpFolder <> "/404.html") html
```

**Step 2: Update imports**

Add `prepare404Context` to Utils imports.

**Step 3: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add src/Main.purs
git commit -m "feat: add 404 page generation with Handlebars"
```

---

## Task 14: Update buildSite to Use New Functions

**Files:**
- Modify: `src/Main.purs`

**Step 1: Rewrite buildSite function**

```purescript
buildSite :: AppM Unit
buildSite = do
  config <- ask
  liftAppM $ Logs.logInfo "Starting..."
  liftAppM $ createFolderIfNotPresent tmpFolder
  liftAppM $ Logs.logInfo "Loading templates..."
  templates <- loadTemplates
  liftAppM $ Logs.logSuccess "Templates loaded."
  liftAppM $ Logs.logInfo "Generating home page..."
  { published, draft } <- generatePostsHTML templates.post
  _ <- createHomePage templates.index published
  liftAppM $ Logs.logSuccess "Home page generated."
  liftAppM $ Logs.logInfo "Generating archive page..."
  _ <- writeArchiveByYearPage templates.archive published
  liftAppM $ Logs.logSuccess "Archive page generated."
  liftAppM $ Logs.logInfo "Generating RSS feed..."
  _ <- Rss.generateRSSFeed (take 10 published)
  liftAppM $ Logs.logSuccess "RSS feed generated."
  liftAppM $ Logs.logInfo "Generating posts..."
  liftAppM $ Logs.logSuccess "Posts generated."
  liftAppM $ Logs.logInfo "Generating 404 page..."
  _ <- write404Page templates.notFound
  liftAppM $ Logs.logSuccess "404 page generated."
  liftAppM $ Logs.logInfo "Copying images folder..."
  _ <- liftEffect $ execSync ("cp -r " <> "./images " <> tmpFolder) defaultExecSyncOptions
  liftAppM $ Logs.logSuccess "images folder copied."
  liftAppM $ Logs.logInfo "Copying js folder..."
  _ <- liftAppM $ liftEffect $ execSync ("cp -r " <> "./js " <> tmpFolder) defaultExecSyncOptions
  liftAppM $ Logs.logSuccess "js folder copied."
  liftAppM $ Logs.logInfo "Generating styles.css..."
  _ <- generateStyles
  liftAppM $ Logs.logSuccess "styles.css generated."
  _ <- removeDraftsFromOutput draft
  liftAppM $ Logs.logInfo $ "Copying " <> tmpFolder <> " to " <> config.outputFolder
  _ <- liftAppM $ createFolderIfNotPresent config.outputFolder
  _ <- liftAppM $ liftEffect $ execSync ("cp -r " <> tmpFolder <> "/* " <> config.outputFolder) defaultExecSyncOptions
  liftAppM $ Logs.logSuccess "Copied."
  liftAppM $ Logs.logInfo "Updating cache..."
  _ <- Cache.writeCacheData
  liftAppM $ Logs.logSuccess "Cached updated."
```

**Step 2: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add src/Main.purs
git commit -m "feat: update buildSite to use Handlebars workflow"
```

---

## Task 15: Remove Dead Code

**Files:**
- Modify: `src/Main.purs`

**Step 1: Remove unused functions**

Delete these functions from Main.purs:
- `recentPosts`
- `replaceContentInTemplate`
- `groupedPostsToHTML`

Also remove from Utils.purs:
- `getCategoriesJson` (no longer used)

**Step 2: Remove unused imports**

Clean up imports that are no longer needed.

**Step 3: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add src/Main.purs src/Utils.purs src/Utils.js
git commit -m "chore: remove dead code after Handlebars migration"
```

---

## Task 16: Update Config Type (Remove totalRecentPosts)

**Files:**
- Modify: `src/Types.purs`
- Modify: `src/Utils.purs`

**Step 1: Remove totalRecentPosts from Config type**

In `src/Types.purs`, update Config:

```purescript
type Config =
  { templateFolder :: String
  , outputFolder :: String
  , contentFolder :: String
  , blogPostTemplate :: String
  , domain :: Maybe String
  }
```

**Step 2: Remove from getConfig in Utils.purs**

Update `getConfig`:

```purescript
getConfig :: forall m. (MonadEffect m) => m Config
getConfig = liftEffect $ do
  templateFolder <- lookupEnv "TEMPLATE_DIR" >>= (pure <$> fromMaybe defaultTemplateFolder)
  outputFolder <- lookupEnv "OUTPUT_DIR" >>= (pure <$> fromMaybe defaultOutputFolder)
  contentFolder <- lookupEnv "POSTS_DIR" >>= (pure <$> fromMaybe defaultContentFolder)
  domain <- lookupEnv "SITE_URL" >>= (\v -> pure $ (dropLeadingSlash <$> v))
  pure $ { domain: domain, templateFolder: templateFolder, outputFolder: outputFolder, contentFolder: contentFolder, blogPostTemplate: defaultBlogpostTemplate templateFolder }
```

Remove `defaultTotalRecentPosts` and related code.

**Step 3: Verify compilation**

Run: `spago build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add src/Types.purs src/Utils.purs
git commit -m "chore: remove totalRecentPosts from Config (user controls in template)"
```

---

## Task 17: End-to-End Test

**Step 1: Build the bundled application**

Run: `yarn bundle`
Expected: Builds successfully, creates `dist/app.cjs`

**Step 2: Create a test directory**

```bash
mkdir -p /tmp/vaak-test && cd /tmp/vaak-test
```

**Step 3: Initialize a new project**

Run: `node /path/to/vaak/dist/app.cjs init`
Expected: Creates templates folder with `.hbs` files

**Step 4: Verify templates created**

```bash
ls templates/
```
Expected: `index.hbs`, `post.hbs`, `archive.hbs`, `404.hbs`, `feed.xml`, `post.md`, `style.css`

**Step 5: Create a test post**

Run: `node /path/to/vaak/dist/app.cjs new test-post`
Expected: Creates `posts/test-post.md`

**Step 6: Edit the test post**

Change status from `draft` to `published` in `posts/test-post.md`

**Step 7: Build the site**

Run: `SITE_URL=https://test.com node /path/to/vaak/dist/app.cjs build`
Expected: Builds successfully, creates files in `public/`

**Step 8: Verify output**

Check `public/index.html` contains the test post in the list.
Check `public/test-post.html` exists with correct content.
Check `public/archive.html` contains the post grouped by year.
Check `public/404.html` exists.

**Step 9: Commit**

```bash
git add -A
git commit -m "feat: complete Handlebars templating migration"
```

---

## Task 18: Update README

**Files:**
- Modify: `readme.md`

**Step 1: Update project structure section**

Update the templates section to show `.hbs` files:

```markdown
## Project Structure

```
your-blog/
├── posts/          # Your markdown posts
├── templates/      # Handlebars templates (.hbs files)
├── images/         # Image assets
├── js/             # JavaScript files
└── public/         # Generated blog
```
```

**Step 2: Add templating section**

Add documentation about Handlebars templates and available data.

**Step 3: Remove RECENT_POSTS documentation**

Remove references to `RECENT_POSTS` env variable since it's no longer used.

**Step 4: Commit**

```bash
git add readme.md
git commit -m "docs: update README for Handlebars templating"
```

---

## Summary

This plan migrates vaak from hard-coded HTML generation to Handlebars templating in 18 tasks:

1. Add handlebars dependency
2. Create Handlebars FFI module
3. Update Templates.purs with .hbs content
4. Update file paths to .hbs
5. Add data preparation functions (JS)
6. Add FFI declarations for data prep
7. Update initApp for .hbs files
8. Add template loading function
9. Refactor generatePostHTML
10. Refactor generatePostsHTML
11. Refactor createHomePage
12. Refactor writeArchiveByYearPage
13. Add 404 page generation
14. Update buildSite orchestration
15. Remove dead code
16. Clean up Config type
17. End-to-end test
18. Update documentation
