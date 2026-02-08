---
title: "Customizing Templates"
date: 2025-01-10
slug: customizing-templates
status: published
collections: Feature
---

vāk uses [Nunjucks](https://mozilla.github.io/nunjucks/) for templating. You have full control over your blog's HTML.

## Template files

When you run `vāk init`, these templates are created in `templates/`:

| File | Purpose |
|------|---------|
| `index.html` | Homepage |
| `post.html` | Individual post pages |
| `archive.html` | Archive page with posts grouped by year |
| `collection.html` | Collection pages |
| `404.html` | Error page |
| `feed.xml` | RSS feed |
| `style.css` | Stylesheet |
| `post.md` | Template for new posts |

## Available data

Each template receives specific data when rendered.

### index.html

- `allPosts` - All published posts (newest first)
- `postsByYear` - Posts grouped by year
- `collections` - All collections
- `siteUrl`, `siteName`

### post.html

- `title`, `date`, `slug`, `content`
- `collections` - Collections this post belongs to
- `siteUrl`, `siteName`

### archive.html

- `postsByYear` - Posts grouped by year
- `siteUrl`, `siteName`

### collection.html

- `collection` - The collection (`name` and `slug`)
- `posts` - Posts in this collection

### 404.html

- `siteUrl`, `siteName`

## Nunjucks basics

```nunjucks
{# Display a variable #}
{{ title }}

{# Render HTML content (for markdown) #}
{{ content | safe }}

{# Loop through posts #}
{% for post in allPosts %}
  <a href="./{{ post.slug }}">{{ post.title }}</a>
{% endfor %}

{# Conditionals #}
{% if collections %}
  Tagged in {{ collections | length }} collections
{% endif %}

{# Limit to first 5 posts #}
{% for post in allPosts %}
  {% if loop.index <= 5 %}
    {{ post.title }}
  {% endif %}
{% endfor %}
```

## Common customizations

### Add navigation

```html
<nav>
  <a href="/">Home</a>
  <a href="/archive.html">Archive</a>
  <a href="/feed.xml">RSS</a>
</nav>
```

### Show collections on posts

```nunjucks
{% if collections %}
  {% for col in collections %}
    <a href="/collection-{{ col.slug }}.html">{{ col.name }}</a>
  {% endfor %}
{% endif %}
```

### Group posts by year on homepage

```nunjucks
{% for group in postsByYear %}
  <h2>{{ group.year }}</h2>
  {% for post in group.posts %}
    <li><a href="./{{ post.slug }}">{{ post.title }}</a></li>
  {% endfor %}
{% endfor %}
```

For more, see the [Nunjucks documentation](https://mozilla.github.io/nunjucks/templating.html).
