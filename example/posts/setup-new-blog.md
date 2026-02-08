---
title: "How to setup a new blog with vāk"
date: 2025-01-02
slug: setup-new-blog
status: published
collections: Getting started
---

Starting from scratch? Here's how to create a new blog with vāk.

## Steps

1. Create a new directory for your blog:

```bash
mkdir my-blog
cd my-blog
```

2. Download `vaak.cjs` from the [releases page](https://github.com/chandru89new/vaak/releases/latest)

3. Make it executable:

```bash
chmod +x vaak.cjs
```

4. Initialize your blog:

```bash
./vaak.cjs init
```

This creates everything you need:

```
my-blog/
├── posts/          # Your markdown posts
├── templates/      # HTML templates (index, post, archive, etc.)
├── images/         # Image assets
├── js/             # JavaScript files
└── public/         # Generated blog (after build)
```

## Optional: Add vāk to your PATH

For easier access, add an alias to your shell config:

```bash
# Add to ~/.bashrc or ~/.zshrc
alias vāk=/path/to/vaak.cjs
```

Now you can run `vāk build` from anywhere.

## Next steps

- [Write your first post](/write-new-post)
- [Build and publish your blog](/publish-blog)
