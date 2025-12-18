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
├── templates/      # Nunjucks templates (.html files - created by `vaak.cjs init`)
├── images/         # Image assets
├── js/             # JavaScript files
└── public/         # Generated blog
```

You can configure the posts and output directories through environment variables ([see configuration](#configuration)). The `templates/` folder is fixed by convention.

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

## Configuration

vāk uses environment variables for configuration:

- `OUTPUT_DIR`: Directory where the built blog will be generated (default: "./public")
- `POSTS_DIR`: Directory containing your markdown posts (default: "./posts")
- `SITE_URL`: The domain name of the blog (e.g., `https://yourname.com/blog`). Required for generating RSS feed.

## Templating

vāk uses [Nunjucks](https://mozilla.github.io/nunjucks/) for templating, giving you full control over your blog's HTML structure.

### Template Files

When you run `vaak.cjs init`, the following templates are created in your templates directory:

- `index.html` - Homepage template
- `post.html` - Individual post template
- `archive.html` - Archive page template
- `404.html` - 404 error page template
- `feed.xml` - RSS feed template
- `style.css` - Default stylesheet (Tailwind supported automatically)
- `post.md` - Markdown template for new posts

### Available Data

Each template has access to specific data when rendered:

**index.html** (Homepage)

- `allPosts` - Array of all published posts (sorted by date, newest first), each with:
  - `title` - Post title
  - `date` - Formatted date (MMM DD, YYYY)
  - `slug` - Post URL slug
- `siteUrl` - Your site URL (from SITE_URL env variable)

**post.html** (Individual Posts)

- `title` - Post title
- `date` - Formatted date (MMM DD, YYYY)
- `slug` - Post URL slug
- `content` - Rendered HTML content (use `| safe` filter to render unescaped)
- `siteUrl` - Your site URL

**archive.html** (Archive Page)

- `postsByYear` - Array of year groups, each with:
  - `year` - Year number
  - `posts` - Array of posts in that year, each with:
    - `title` - Post title
    - `date` - Formatted date (MMM DD, YYYY)
    - `slug` - Post URL slug
- `siteUrl` - Your site URL

**404.html** (404 Error Page)

- `siteUrl` - Your site URL

### Nunjucks Syntax

Templates use standard Nunjucks syntax. Common patterns:

```nunjucks
<!-- Display a variable -->
{{ title }}

<!-- Display unescaped HTML (for rendered markdown content) -->
{{ content | safe }}

<!-- Loop through posts -->
{% for post in allPosts %}
  <li><a href="./{{ post.slug }}">{{ post.title }}</a> - {{ post.date }}</li>
{% endfor %}

<!-- Limit to first 5 posts -->
{% for post in allPosts %}{% if loop.index <= 5 %}
  <li>{{ post.title }}</li>
{% endif %}{% endfor %}

<!-- Nested loops (archive page) -->
{% for group in postsByYear %}
  <h3>{{ group.year }}</h3>
  {% for post in group.posts %}
    <li>{{ post.title }}</li>
  {% endfor %}
{% endfor %}
```

For more information on Nunjucks syntax and features, visit the [Nunjucks documentation](https://mozilla.github.io/nunjucks/templating.html).

## Colophon

वाक् (vāk) is Sanskrit for "speech" or "voice".
