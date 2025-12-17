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
├── templates/      # Handlebars templates (.hbs files - created by `vaak.cjs init`)
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
status: published -- can be "draft", "published" or "unlisted". "draft" wont be generated in the build. "unlisted" wont be added to the homepage, archive page or the RSS feeds but can be accessed by the URL/slug.
slug: my-first-post
---

Your post content here...
```

Note: The `slug` should be the same as the filename without the `.md` extension.

## Configuration

vāk uses environment variables for configuration:

- `TEMPLATE_DIR`: Directory containing Handlebars templates (default: "./templates")
- `OUTPUT_DIR`: Directory where the built blog will be generated (default: "./public")
- `POSTS_DIR`: Directory containing your markdown posts (default: "./posts")
- `SITE_URL`: The domain name of the blog. (eg. `https://yourname.com/blog`) This is required for generating RSS feed anytime you build the blog.

## Templating

vāk uses [Handlebars](https://handlebarsjs.com/) for templating, giving you full control over your blog's HTML structure.

### Template Files

When you run `vaak.cjs init`, the following Handlebars templates are created in your templates directory:

- `index.hbs` - Homepage template
- `post.hbs` - Individual post template
- `archive.hbs` - Archive page template
- `404.hbs` - 404 error page template
- `feed.xml` - RSS feed template
- `style.css` - Default stylesheet
- `post.md` - Markdown template for new posts

### Available Data

Each template has access to specific data when rendered:

**index.hbs** (Homepage)
- `allPosts` - Array of all published posts, each with:
  - `title` - Post title
  - `date` - Formatted date (MMM DD, YYYY)
  - `slug` - Post URL slug
- `siteUrl` - Your site URL (from SITE_URL env variable)

**post.hbs** (Individual Posts)
- `title` - Post title
- `date` - Formatted date (MMM DD, YYYY)
- `slug` - Post URL slug
- `content` - Rendered HTML content (use triple braces: `{{{content}}}`)
- `siteUrl` - Your site URL

**archive.hbs** (Archive Page)
- `postsByYear` - Array of year groups, each with:
  - `year` - Year number
  - `posts` - Array of posts in that year, each with:
    - `title` - Post title
    - `date` - Formatted date (MMM DD, YYYY)
    - `slug` - Post URL slug
- `siteUrl` - Your site URL

**404.hbs** (404 Error Page)
- `siteUrl` - Your site URL

### Handlebars Syntax

Templates use standard Handlebars syntax. Common patterns:

```handlebars
<!-- Display a variable -->
{{title}}

<!-- Display unescaped HTML -->
{{{content}}}

<!-- Loop through posts -->
{{#each allPosts}}
  <li><a href="./{{slug}}">{{title}}</a> - {{date}}</li>
{{/each}}

<!-- Nested loops -->
{{#each postsByYear}}
  <h3>{{year}}</h3>
  {{#each posts}}
    <li>{{title}}</li>
  {{/each}}
{{/each}}
```

For more information on Handlebars syntax and features, visit the [Handlebars documentation](https://handlebarsjs.com/guide/).

## Colophon

वाक् (vāk) is Sanskrit for "speech" or "voice".
