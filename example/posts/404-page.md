---
title: "The 404 Page"
date: 2025-01-16
slug: 404-page
status: published
collections: Feature
---

vāk generates a custom 404 error page for your blog.

## What is it?

The `404.html` page is shown when visitors try to access a URL that doesn't exist. Instead of a generic server error, they see your custom page.

## Customizing it

Edit `templates/404.html` to match your blog's style:

```html
<!DOCTYPE html>
<html>
<head>
  <title>Page Not Found - {{ siteName }}</title>
  <link rel="stylesheet" href="/style.css">
</head>
<body>
  <h1>404 - Page Not Found</h1>
  <p>The page you're looking for doesn't exist.</p>
  <p><a href="/">Go back home</a></p>
</body>
</html>
```

## Available data

The 404 template receives:

- `siteUrl` - Your site URL
- `siteName` - Your site name

## Deployment notes

How the 404 page works depends on your hosting provider:

### GitHub Pages

Automatically serves `404.html` for missing pages. No configuration needed.

### Netlify

Automatically serves `404.html`. You can also add a `_redirects` file for more control.

### Vercel

Add a `vercel.json` to your output directory:

```json
{
  "routes": [
    { "handle": "filesystem" },
    { "src": "/(.*)", "dest": "/404.html", "status": 404 }
  ]
}
```

### Apache

Add to `.htaccess`:

```
ErrorDocument 404 /404.html
```

### Nginx

Add to your server config:

```
error_page 404 /404.html;
```

## Testing locally

With `npx serve public`, visiting a non-existent URL won't show your 404 page (serve doesn't support custom error pages). Deploy to test the actual behavior.
