module RssGenerator where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array (foldl, head)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Exception (error)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Types (FormattedMarkdownData, FrontMatterS)
import Utils (askConfig, md2FormattedData)
import Utils as Utils

feedItemTemplate ∷ String
feedItemTemplate =
  """<item>
<title>{{title}}</title>
<link>{{post_url}}</link>
<guid>{{post_url}}</guid>
<description><![CDATA[{{content}}]]></description>
<pubDate>{{published_date}}</pubDate>
</item>"""

generateRSSFeed :: Array FrontMatterS -> Aff Unit
generateRSSFeed fds = do
  config <- askConfig
  templateContents <- readTextFile UTF8 (config.templateFolder <> "/feed.xml")
  parsedContents <- parTraverse (\fm -> md2FormattedData <$> readTextFile UTF8 (config.contentFolder <> "/" <> fm.slug <> ".md")) fds
  case config.domain of
    Nothing -> do
      throwError $ error "Cannot generate RSS feed without a SITE_URL set in the environment. (e.g SITE_URL=https://my.blog)."
    Just domain -> do
      feedItemsString <- pure $ generateFeedItemString domain feedItemTemplate parsedContents
      lastUpdated <- pure $ getLastUpdated (head fds)
      updatedFeedContents <- pure $ replaceFeedContents feedItemsString lastUpdated templateContents
      writeTextFile UTF8 (Utils.tmpFolder <> "/feed.xml") updatedFeedContents

generateFeedItemString :: String -> String -> Array FormattedMarkdownData -> String
generateFeedItemString domain template = foldl fn ""
  where
  fn :: String -> FormattedMarkdownData -> String
  fn b fd =
    b
      <>
        ( replaceAll (Pattern "{{title}}") (Replacement $ formatTitle fd.frontMatter.title) template
            # replaceAll (Pattern "{{post_url}}") (Replacement (domain <> "/" <> fd.frontMatter.slug))
            # replaceAll (Pattern "{{content}}") (Replacement fd.content)
            # replaceAll (Pattern "{{published_date}}") (Replacement $ Utils.formatDate rssDateFormat fd.frontMatter.date)
        )

getLastUpdated :: Maybe FrontMatterS -> String
getLastUpdated Nothing = ""

getLastUpdated (Just fd) = Utils.formatDate rssDateFormat fd.date

replaceFeedContents :: String -> String -> String -> String
replaceFeedContents items lastUpdatedAt template =
  replaceAll (Pattern "{{last_updated_date}}") (Replacement lastUpdatedAt) template
    # replaceAll (Pattern "{{feed_items}}") (Replacement items)

test ∷ Effect Unit
test =
  launchAff_
    $ do
        _ <- generateRSSFeed []
        log "Done."

formatTitle :: String -> String
formatTitle str = replaceAll (Pattern "&") (Replacement "&amp;") str

rssDateFormat ∷ String
rssDateFormat = "ddd, DD MMM YYYY hh:mm:ss ZZ"
