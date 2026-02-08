---
title: "How to setup vāk in an existing directory"
date: 2025-01-03
slug: setup-existing-directory
status: published
collections: Getting started
---

Already have a folder with content? Here's how to add vāk to it.

## Steps

1. Download `vaak.cjs` from the [releases page](https://github.com/chandru89new/vaak/releases/latest)

2. Place it in your project directory

3. Make it executable:

```bash
chmod +x vaak.cjs
```

4. Initialize vāk:

```bash
./vaak.cjs init
```

This creates the required folder structure:

```
your-project/
├── posts/          # Your markdown posts go here
├── templates/      # Nunjucks templates
├── images/         # Image assets
├── js/             # JavaScript files
└── public/         # Generated output (created on build)
```

## What if I already have a posts folder?

vāk won't overwrite existing files. If you have a `posts/` directory, it stays untouched. The `init` command only creates what's missing.

## What if my posts are in a different directory?

If your markdown files live somewhere other than `posts/`, use the `POSTS_DIR` environment variable:

```bash
export POSTS_DIR=./content
./vaak.cjs build
```

This tells vāk to look for posts in `./content` instead of the default `./posts`.

For example, if your existing structure looks like this:

```
my-project/
├── articles/       # Your existing markdown files
├── assets/
└── ...
```

Just set `POSTS_DIR` to point to your articles folder:

```bash
export POSTS_DIR=./articles
./vaak.cjs build
```

Add this to your shell config (`~/.bashrc` or `~/.zshrc`) to make it permanent.

## Next steps

- [Write your first post](/write-new-post)
- [Build and publish your blog](/publish-blog)
