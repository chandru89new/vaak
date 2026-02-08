---
title: "Styling Your Blog"
date: 2025-01-11
slug: styling-your-blog
status: published
collections: Feature
---

vāk uses a `style.css` file in your templates directory for styling.

## How it works

The `templates/style.css` file is copied to your output directory during build. All your HTML templates can reference it:

```html
<link rel="stylesheet" href="/style.css">
```

## Basic customization

Edit `templates/style.css` to change your blog's appearance:

```css
/* Typography */
body {
  font-family: Georgia, serif;
  line-height: 1.6;
  color: #333;
}

/* Links */
a {
  color: #0066cc;
}

/* Post titles */
h1 {
  font-size: 2rem;
  margin-bottom: 0.5rem;
}

/* Code blocks */
pre {
  background: #f5f5f5;
  padding: 1rem;
  overflow-x: auto;
}
```

## Using Tailwind CSS

vāk has built-in Tailwind support. Just use Tailwind classes in your `templates/style.css`:

```css
@tailwind base;
@tailwind components;
@tailwind utilities;

/* Your custom styles */
.post-title {
  @apply text-3xl font-bold mb-4;
}

.post-date {
  @apply text-gray-600 mb-8;
}

.prose {
  @apply max-w-2xl mx-auto px-4 py-8;
}
```

vāk automatically processes Tailwind directives during build - no separate build step needed.

## Adding custom fonts

### Google Fonts

```html
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap" rel="stylesheet">
```

```css
body {
  font-family: 'Inter', sans-serif;
}
```

### Self-hosted fonts

Add font files to your `images/` or `js/` directory and reference them:

```css
@font-face {
  font-family: 'MyFont';
  src: url('/images/myfont.woff2') format('woff2');
}
```

## Dark mode

Add a dark mode with CSS:

```css
@media (prefers-color-scheme: dark) {
  body {
    background: #1a1a1a;
    color: #e0e0e0;
  }
  a {
    color: #6db3f2;
  }
}
```

This automatically switches based on the user's system preference.
