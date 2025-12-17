module Templates where

import Prelude

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

postMdTemplate :: String
postMdTemplate =
  """---
title: $title
date: $date
slug: $slug
status: draft
---

Write your post here.
"""

feedTemplate :: String -> String
feedTemplate domain =
  one <> "<link>" <> domain <> "/</link>" <> two <> "<atom:link href=\"" <> domain <> "/feed.xml\" rel=\"self\" type=\"application/rss+xml\"" <> "/>" <> three 
  where
    one = """<?xml version="1.0" encoding="utf-8"?><rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom"><channel><title>my blog — RSS Feed</title>"""
    two = """<description>this is my blog</description>
    <lastBuildDate>{{last_updated_date}}</lastBuildDate>"""
    three = """{{feed_items}}
  </channel>
</rss>"""
--   """<?xml version="1.0" encoding="utf-8"?>
-- <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
--   <channel>
--     <title>my blog — RSS Feed</title>
--     <link>https://my.blog.com/</link>
--     <description>this is my blog</description>
--     <lastBuildDate>{{last_updated_date}}</lastBuildDate>
--     <atom:link href="https://my.blog.com/feed.xml" rel="self" type="application/rss+xml" />
--     """

styleTemplate :: String
styleTemplate =
  """@import url("https://fonts.googleapis.com/css2?family=Fraunces:ital,opsz,wght,SOFT@0,9..144,100..900,50;1,9..144,100..900,50&display=swap");
@import url("https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/github.min.css");

@import "tailwindcss";

html {
  @apply text-[16px] md:text-[20px];
}

h1,
h2,
h3,
h4,
h5,
h6 {
  @apply font-semibold;
}

body {
  font-family: "Fraunces", "Atkinson Hyperlegible", system-ui, -apple-system,
    BlinkMacSystemFont, "Segoe UI", Roboto, Oxygen, Ubuntu, Cantarell,
    "Open Sans", "Helvetica Neue", sans-serif;
  font-weight: 300;
  @apply bg-slate-50 text-slate-800 p-5 md:p-10 max-w-[700px] leading-relaxed dark:bg-slate-700 dark:text-slate-200;
}

a {
  @apply text-lime-600 hover:text-lime-900 dark:hover:text-lime-700 transition;
}

article:not(#archive_container) a {
  /* @apply border-b-lime-600 hover:border-b-lime-900 dark:border-b-lime-700; */
  @apply border-b border-dotted border-b-lime-500 hover:border-b-lime-900 dark:border-b-lime-700;
}

ul {
  @apply space-y-2 sm:space-y-1;
}

h1 {
  @apply font-semibold tracking-tight text-3xl;
}

#logo,
#logo a {
  @apply text-slate-400;
  font-family: "Fraunces", serif;
}

#home-page-logo a {
  @apply block font-semibold tracking-tight text-3xl;
  font-family: "Fraunces", serif;
}

article {
  @apply space-y-6;
}

#date {
  @apply text-sm text-slate-500;
}

header {
  @apply mt-3 mb-4 pb-4 border-b dark:border-b-slate-600;
}

footer {
  @apply text-sm sm:text-base pt-4 mt-4 mb-10 border-t flex gap-x-3 dark:border-t-slate-600;
}

section h3 {
  @apply font-semibold mb-1 uppercase text-xs text-slate-400;
  font-family: "Fraunces", serif;
}

.date {
  @apply text-slate-400 text-xs;
  font-family: "Fraunces", serif;
}

/* code, pre {
  font-family: monospace;
  @apply text-xs;
} */

code,
li code {
  @apply font-mono text-xs px-1 py-1 bg-slate-200 dark:bg-slate-800 leading-5 rounded;
}

.hljs {
  @apply rounded border border-slate-200 bg-slate-100 dark:border-slate-900 dark:bg-slate-800 dark:text-slate-400;
}

ol {
  @apply list-decimal;
}

article:not(#archive_container) ul {
  @apply list-disc ml-4;
}

/* disable list-disc for home page list */
section ul {
  @apply list-none;
}

/* disabled list-disc for archive page list */
article#archive_container ul {
  @apply list-none;
}
article#archive_container ul li {
  @apply whitespace-nowrap overflow-x-visible;
}

blockquote {
  @apply border-l-4 border-slate-200 dark:border-slate-600 pl-4 space-y-4 text-slate-500 text-sm dark:text-slate-400 leading-relaxed;
}

hr {
  @apply border-slate-200 dark:border-slate-600;
}

.footnote {
  @apply text-xs;
}"""

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