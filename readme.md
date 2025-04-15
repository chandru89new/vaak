# vaak (vāk) - A minimal, static blog generator

A simple Node.js script for generating a static blog.

Example: [`https://notes.druchan.com`](https://notes.druchan.com)

## Installation

1. Download the latest `vaak.cjs` from the [releases page](https://github.com/chandru89new/vaak/releases/latest)
2. Make it executable:

```bash
chmod +x vaak.cjs
```

3. (Optional) Move it to a directory in your PATH for easier access

## Usage

vāk provides several commands to manage your static blog:

```bash
# Init a new project (or in your existing project)
./vaak.cjs init

# Create a new post
./vaak.cjs new [slug]

# Build the blog
./vaak.cjs build

# Show version information
./vaak.cjs version

# Display help
./vaak.cjs help
```

## Project Structure

```
your-blog/
├── posts/          # Your markdown posts
├── templates/      # HTML templates (this is created by `vaak.cjs init` and you can edit the contents of the files)
├── images/         # Image assets
├── js/             # JavaScript files
└── public/         # Generated blog
```

You can use any directory name for the posts, the templates and the public (output) folders. Just configure them through shell environment variables ([see configuration](#configuration)).

## Writing Posts

Posts should be written in Markdown with front matter. Example:

```markdown
---
title: "My First Post" (use quotes for safety)
date: 2025-04-14 (always YYYY-MM-DD format)
status: published -- can be "draft" or "published". "draft" wont be generated in the build.
slug: my-first-post
---

Your post content here...
```

Note: The `slug` should be the same as the filename without the `.md` extension.

## Configuration

vāk uses environment variables for configuration:

- `TEMPLATE_DIR`: Directory containing HTML templates (default: "./templates")
- `OUTPUT_DIR`: Directory where the built blog will be generated (default: "./public")
- `POSTS_DIR`: Directory containing your markdown posts (default: "./posts")
- `RECENT_POSTS`: Number of recent posts to show on homepage (default: 5)
- `SITE_URL`: The domain name of the blog. (eg. `https://yourname.com/blog`) This is required for generating RSS feed anytime you build the blog.

## Colophon

वाक् (vāk) is Sanskrit for "speech" or "voice".