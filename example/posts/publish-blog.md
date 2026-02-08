---
title: "How to publish your blog"
date: 2025-01-05
slug: publish-blog
status: published
collections: Getting started
---

Here's how to build and deploy your vāk blog.

## Set environment variables

vāk needs two environment variables:

```bash
export SITE_URL=https://yourdomain.com
export SITE_NAME="Your Blog Name"
```

Add these to your shell config (`~/.bashrc` or `~/.zshrc`) to make them permanent.

### Optional variables

| Variable | Default | Description |
|----------|---------|-------------|
| `OUTPUT_DIR` | `./public` | Where generated files go |
| `POSTS_DIR` | `./posts` | Where your markdown posts are |

## Build the blog

Run the build command:

```bash
./vaak.cjs build
```

This generates your static blog in the `public/` directory (or your custom `OUTPUT_DIR`).

## Preview locally

Before deploying, preview your blog locally:

```bash
npx serve public
```

Open `http://localhost:3000` to see your blog.

## Deploy

The `public/` folder contains plain HTML, CSS, and JavaScript. Deploy it anywhere that serves static files:

### GitHub Pages

Push the `public/` folder to a `gh-pages` branch or configure GitHub Pages to serve from a directory.

### Netlify / Vercel

Connect your repository and set the publish directory to `public/`.

### Any static host

Upload the contents of `public/` to your web server.

## Rebuild when you make changes

After editing posts or templates, run `./vaak.cjs build` again to regenerate your blog.
