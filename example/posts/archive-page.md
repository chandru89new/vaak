---
title: "The Archive Page"
date: 2025-01-15
slug: archive-page
status: published
collections: Feature
---

vāk generates an archive page that lists all your posts grouped by year.

## How it works

When you run `vāk build`, it creates `archive.html` in your output directory. This page shows all published posts organized chronologically.

## Linking to the archive

Add a link to your archive from your homepage or navigation:

```html
<a href="/archive.html">Archive</a>
```

## Available data

The archive template receives:

- `postsByYear` - Posts grouped by year
- `siteUrl` - Your site URL
- `siteName` - Your site name

### postsByYear structure

```javascript
[
  {
    year: 2025,
    posts: [
      { title: "...", date: "Jan 15, 2025", slug: "...", collections: [...] },
      { title: "...", date: "Jan 10, 2025", slug: "...", collections: [...] }
    ]
  },
  {
    year: 2024,
    posts: [...]
  }
]
```

Posts within each year are sorted newest first.

## Customizing the template

Edit `templates/archive.html` to change the layout:

```nunjucks
<h1>Archive</h1>

{% for group in postsByYear %}
  <section>
    <h2>{{ group.year }}</h2>
    <ul>
      {% for post in group.posts %}
        <li>
          <span>{{ post.date }}</span>
          <a href="./{{ post.slug }}">{{ post.title }}</a>
        </li>
      {% endfor %}
    </ul>
  </section>
{% endfor %}
```

### Show post count per year

```nunjucks
<h2>{{ group.year }} ({{ group.posts | length }} posts)</h2>
```

### Include collections

```nunjucks
{% for post in group.posts %}
  <li>
    <a href="./{{ post.slug }}">{{ post.title }}</a>
    {% if post.collections %}
      <span class="tags">
        {% for col in post.collections %}
          <a href="/collection-{{ col.slug }}.html">{{ col.name }}</a>
        {% endfor %}
      </span>
    {% endif %}
  </li>
{% endfor %}
```

## Archive on the homepage

You can also use `postsByYear` on your homepage to show posts grouped by year instead of a flat list. The same data is available in `index.html`.
