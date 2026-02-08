---
title: "Collections"
date: 2025-01-12
slug: collections
status: published
collections: Feature
---

Collections let you group related posts together. A post can belong to multiple collections.

## How to use

Add a `collections` field to your post's frontmatter:

```yaml
---
title: "My Post"
collections: Journal, Tutorials
---
```

That's it. Your post now belongs to the "Journal" and "Tutorials" collections.

## Viewing collections

Each collection gets its own page at `/collection-{slug}.html`, where `slug` is a URL-safe version of the collection name.

Example: [Feature collection](/collection-feature.html)

## Using collections in templates

Collections are available in these templates: `index.html`, `post.html`, `archive.html`, and `collection.html`.

Each collection has two properties:

| Property | Description |
|----------|-------------|
| `name` | The collection name as written in frontmatter |
| `slug` | URL-safe version of the name |

Example usage in `post.html`:

```html
{% for collection in collections %}
  <a href="/collection-{{collection.slug}}.html">{{collection.name}}</a>
{% endfor %}
```
