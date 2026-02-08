---
title: "RSS Feeds"
date: 2025-01-17
slug: rss-feeds
status: published
collections: Feature
---

vāk automatically generates an RSS feed for your blog.

## How it works

When you run `vāk build`, it creates `feed.xml` in your output directory. This file contains your published posts in RSS format, which readers can subscribe to using feed readers like Feedly, NetNewsWire, or Inoreader.

## Subscription URL

Your feed is available at:

```
https://yourdomain.com/feed.xml
```

Link to it from your templates so readers can find it:

```html
<a href="/feed.xml">Subscribe via RSS</a>
```

## What's included

- **Published posts** - All posts with `status: published`
- **Excluded** - Posts with `status: draft` or `status: unlisted`

Posts appear in reverse chronological order (newest first).

## Customizing the feed

Edit `templates/feed.xml` to customize your feed. The template receives the same `allPosts` data as the homepage.

Common customizations:

- Change the feed title and description
- Adjust how many posts appear in the feed
- Modify the content format

## Adding feed autodiscovery

Help feed readers find your RSS automatically by adding this to your HTML `<head>`:

```html
<link rel="alternate" type="application/rss+xml"
      title="{{ siteName }}" href="{{ siteUrl }}/feed.xml">
```
