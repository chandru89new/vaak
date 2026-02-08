---
title: "How to write a new post"
date: 2025-01-04
slug: write-new-post
status: published
collections: Getting started
---

Here's how to create and write posts in vāk.

## Create a new post

Use the `new` command with a slug (URL-friendly name):

```bash
./vaak.cjs new my-first-post
```

This creates `posts/my-first-post.md` with a template you can edit.

## Post structure

Every post needs frontmatter at the top:

```markdown
---
title: "My First Post"
date: 2025-01-15
slug: my-first-post
status: published
collections: Journal, Tutorials
---

Your content starts here...
```

### Frontmatter fields

| Field | Required | Description |
|-------|----------|-------------|
| `title` | Yes | Post title (use quotes for safety) |
| `date` | Yes | Publication date in YYYY-MM-DD format |
| `slug` | Yes | URL-friendly identifier |
| `status` | Yes | `published`, `draft`, or `unlisted` |
| `collections` | No | Comma-separated list of collections |

### Status options

- **published** - Appears everywhere (homepage, archive, RSS)
- **draft** - Not generated at all
- **unlisted** - Accessible by URL but hidden from listings

## Writing content

Write your post in Markdown. Standard syntax works:

```markdown
## Headings

Paragraphs with **bold** and *italic* text.

- Bullet lists
- Like this

1. Numbered lists
2. Work too

[Links](https://example.com) and `inline code`.

Code blocks with syntax highlighting:

​```javascript
const greeting = "Hello, world!";
​```
```

## Next steps

After writing, [build and publish your blog](/publish-blog).
