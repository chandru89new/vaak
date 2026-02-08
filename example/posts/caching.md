---
title: "Caching"
date: 2025-01-18
slug: caching
status: published
collections: Feature
---

vāk caches builds to speed up your workflow. Unchanged files don't need re-building, so subsequent builds are faster.

## How it works

Build cache data is stored in a `.cache` file in your project directory. When you run `vāk build`, only modified posts and templates are processed.

## Rebuilding everything

To force a complete rebuild:

```bash
vāk rebuild
```

This removes the `.cache` file and rebuilds the entire site.

## Common scenarios

**Deleted a post but its HTML file still exists?**

This happens because the cache doesn't track deletions. Either remove the HTML file directly from your output directory, or use `vāk rebuild` to rebuild from scratch which will remove the deleted posts' HTML files.

**Updated a template but changes aren't showing?**

Template changes may not invalidate the cache properly. Run a full rebuild:

```bash
vāk rebuild
```
