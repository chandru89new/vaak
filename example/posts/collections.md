---
title: "Collections"
date: 2025-01-02
slug: collections
status: published
collections: Docs
---

Collections let you group related posts together.

## How to use

Add a `collections` field to your post's frontmatter:

```yaml
---
title: "My Post"
collections: Journal, Tutorials
---
```

That's it. Your post now belongs to the "journal" and "tutorials" collections.

## Viewing collections

Each collection gets its own page at `/collections/collection-<slug>`, where `slug` is a websafe/URL-safe name of the collection. All posts tagged with that collection are listed there.

Example: [Docs collection](/collection-docs)
