---
title: "Adding Images"
date: 2025-01-13
slug: adding-images
status: published
collections: Feature
---

vāk includes an `images/` directory for your image assets.

## Using images in posts

1. Add your image to the `images/` folder
2. Reference it in your markdown:

```markdown
![Alt text](/images/my-photo.jpg)
```

Or with a relative path:

```markdown
![Alt text](../images/my-photo.jpg)
```

## Adding a favicon

The default templates look for a favicon at `images/favicon.png`. To add one:

1. Create a square image (32x32 or 64x64 pixels works well)
2. Save it as `images/favicon.png`

The favicon link in your templates:

```html
<link rel="icon" href="/images/favicon.png">
```

## Image optimization

vāk copies images as-is without processing. For best performance:

- **Compress images** before adding them (use tools like ImageOptim, Squoosh, or TinyPNG)
- **Use appropriate formats** - JPEG for photos, PNG for graphics with transparency, WebP for modern browsers
- **Size appropriately** - Don't upload 4000px images if they'll display at 800px

## Using images in templates

Reference images in your HTML templates:

```html
<img src="/images/logo.png" alt="Site logo">
```

Or with the site URL for absolute paths:

```html
<img src="{{ siteUrl }}/images/header.jpg" alt="Header">
```
