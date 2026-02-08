---
title: "Adding JavaScript"
date: 2025-01-14
slug: adding-javascript
status: published
collections: Feature
---

vāk includes a `js/` directory for JavaScript files.

## Adding scripts

1. Add your JavaScript file to the `js/` folder
2. Include it in your templates:

```html
<script src="/js/my-script.js"></script>
```

Add this before the closing `</body>` tag in your templates.

## Common use cases

### Analytics

Add tracking scripts to monitor your blog's traffic:

```html
<!-- In templates/post.html and templates/index.html -->
<script async src="https://analytics.example.com/script.js"></script>
```

### Comments

Add a commenting system like [Utterances](https://utteranc.es/) (uses GitHub issues):

```html
<!-- In templates/post.html -->
<script src="https://utteranc.es/client.js"
        repo="username/repo"
        issue-term="pathname"
        theme="github-light"
        crossorigin="anonymous"
        async>
</script>
```

### Syntax highlighting

Enhance code blocks with [Prism.js](https://prismjs.com/) or [highlight.js](https://highlightjs.org/):

```html
<link rel="stylesheet" href="/js/prism.css">
<script src="/js/prism.js"></script>
```

### Dark mode toggle

```html
<script src="/js/theme.js"></script>
```

## Per-template scripts

You can add scripts to specific templates:

- `index.html` - Homepage-only scripts
- `post.html` - Post page scripts (comments, reading time)
- `archive.html` - Archive-specific functionality

## External scripts

For third-party scripts, link directly:

```html
<script src="https://cdn.example.com/library.min.js"></script>
```
