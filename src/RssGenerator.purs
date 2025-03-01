module RssGenerator where

import Prelude
import Data.Array (foldl, head)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Utils (FormattedMarkdownData, templatesFolder)
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

generateRSSFeed :: Array FormattedMarkdownData -> Aff Unit
generateRSSFeed fds = do
  templateContents <- readTextFile UTF8 (templatesFolder <> "/feed.xml")
  feedItemsString <- pure $ generateFeedItemString feedItemTemplate fds
  lastUpdated <- pure $ getLastUpdated (head fds)
  updatedFeedContents <- pure $ replaceFeedContents feedItemsString lastUpdated templateContents
  writeTextFile UTF8 (Utils.tmpFolder <> "/feed.xml") updatedFeedContents

generateFeedItemString :: String -> Array FormattedMarkdownData -> String
generateFeedItemString template = foldl fn ""
  where
  fn b fd =
    b
      <>
        ( replaceAll (Pattern "{{title}}") (Replacement $ formatTitle fd.frontMatter.title) template
            # replaceAll (Pattern "{{post_url}}") (Replacement ("https://notes.druchan.com/" <> fd.frontMatter.slug))
            # replaceAll (Pattern "{{content}}") (Replacement fd.content)
            # replaceAll (Pattern "{{published_date}}") (Replacement $ Utils.formatDate rssDateFormat fd.frontMatter.date)
        )

getLastUpdated :: Maybe FormattedMarkdownData -> String
getLastUpdated Nothing = ""

getLastUpdated (Just fd) = Utils.formatDate rssDateFormat fd.frontMatter.date

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
