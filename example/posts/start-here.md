---
title: "Welcome to vāk"
date: 2025-01-01
slug: start-here
status: published
---

This is a blog powered by [vāk](https://github.com/chandru89new/vaak), a minimal static blog generator.

## What is vāk?

vāk is a simple tool that takes your Markdown posts and turns them into a static blog. No databases, no servers, just plain HTML files you can host anywhere.

## Why vāk?

I wrote vāk for a few reasons:

- **Learning PureScript** - vāk is written in PureScript, a strongly-typed functional language that compiles to JavaScript. Building a real tool was a great way to learn.
- **Simplicity** - I wanted full control over my blog without the complexity of larger static site generators. vāk does one thing: convert Markdown to HTML.
- **Control** - With vāk, you control the input (Markdown files), the output (HTML), and everything in between (templates). No magic, no hidden complexity.

## Try it yourself

You're looking at the example folder right now. Here's how to build this blog:

1. Download `vāk.cjs` from the [releases page](https://github.com/chandru89new/vaak/releases/latest)
2. Place it in this folder
3. Run the build:

```bash
export SITE_URL=https://example.com
export SITE_NAME="My Blog"
./vaak.cjs build
```

This creates a `public` folder with your generated blog. To preview it locally:

```bash
npx serve public
```

Then open `http://localhost:3000` in your browser.

## Create your next post

This very post comes from `posts/start-here.md`. Want to add another?

```bash
./vaak.cjs new my-next-post
```

This creates `posts/my-next-post.md`. Edit it, then rebuild:

```bash
./vaak.cjs build
```

Your new post will appear in the `public` folder.

## What's next?

- Edit the templates in `templates/` to customize your blog's look
- Check out the [README](https://github.com/chandru89new/vaak) for more details
- Start writing!
