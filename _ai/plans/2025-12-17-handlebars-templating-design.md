# Handlebars Templating for vaak

## Overview

Replace hard-coded HTML generation in vaak with proper Handlebars templating. vaak prepares data as JSON-like structures and passes them to Handlebars templates to produce final HTML output.

## Template Files

After `vaak init`, the templates folder contains:

```
templates/
├── index.hbs      # Homepage
├── post.hbs       # Individual blog post
├── archive.hbs    # Archive page
├── 404.hbs        # Not found page
├── feed.xml       # RSS (unchanged, uses {{placeholders}})
├── post.md        # New post template (unchanged, uses $placeholders)
└── style.css      # Styles (unchanged)
```

- Four `.hbs` files are Handlebars templates
- `feed.xml`, `post.md`, `style.css` remain unchanged

## Data Shapes

Each template receives a context object.

### post.hbs

```json
{
  "title": "My Post Title",
  "date": "Dec 17, 2025",
  "slug": "my-post-title",
  "content": "<p>HTML content...</p>",
  "siteUrl": "https://myblog.com"
}
```

### index.hbs

```json
{
  "allPosts": [
    { "title": "...", "date": "Dec 17, 2025", "slug": "..." },
    { "title": "...", "date": "Dec 10, 2025", "slug": "..." }
  ],
  "siteUrl": "https://myblog.com"
}
```

### archive.hbs

```json
{
  "postsByYear": [
    { "year": 2025, "posts": [{ "title": "...", "date": "...", "slug": "..." }] },
    { "year": 2024, "posts": [...] }
  ],
  "siteUrl": "https://myblog.com"
}
```

### 404.hbs

```json
{
  "siteUrl": "https://myblog.com"
}
```

## Example Templates

### post.hbs

```handlebars
<!DOCTYPE html>
<html lang="en">
<head>
  <title>{{title}} — my blog</title>
  <link href="./style.css" rel="stylesheet" />
</head>
<body>
  <h3><a href="/">← my blog</a></h3>
  <header>
    <h1>{{title}}</h1>
    <div class="date">{{date}}</div>
  </header>
  <article>{{{content}}}</article>
  <footer>
    <a href="/">← blog</a>
  </footer>
</body>
</html>
```

Note: `{{{content}}}` uses triple braces to render HTML unescaped.

### index.hbs

```handlebars
<!DOCTYPE html>
<html lang="en">
<head>
  <title>my blog</title>
  <link href="./style.css" rel="stylesheet" />
</head>
<body>
  <h3><a href="/">my blog</a></h3>
  <section>
    <h3>Recent posts</h3>
    <ul>
      {{#each allPosts}}
      <li><a href="./{{slug}}">{{title}}</a> — <span class="date">{{date}}</span></li>
      {{/each}}
    </ul>
  </section>
  <footer>
    <a href="/archive">archive</a>
    <a href="/feed.xml">rss feed</a>
  </footer>
</body>
</html>
```

### archive.hbs

```handlebars
<!DOCTYPE html>
<html lang="en">
<head>
  <title>Archive — my blog</title>
  <link href="./style.css" rel="stylesheet" />
</head>
<body>
  <h3><a href="/">← my blog</a></h3>
  <header>
    <h1>Archive</h1>
  </header>
  <article>
    {{#each postsByYear}}
    <section>
      <h3>{{year}}</h3>
      <ul>
        {{#each posts}}
        <li><a href="/{{slug}}">{{title}}</a> — <span class="date">{{date}}</span></li>
        {{/each}}
      </ul>
    </section>
    {{/each}}
  </article>
  <footer>
    <a href="/">← blog</a>
    <a href="/feed.xml">rss feed</a>
  </footer>
</body>
</html>
```

### 404.hbs

```handlebars
<!DOCTYPE html>
<html lang="en">
<head>
  <title>Not Found — my blog</title>
  <link href="./style.css" rel="stylesheet" />
</head>
<body>
  <h3><a href="/">my blog</a></h3>
  <article>
    <p>That page doesn't exist. Sorry about that.</p>
    <a href="/archive">See the archive →</a>
  </article>
  <footer>
    <a href="/">← blog</a>
  </footer>
</body>
</html>
```

## Implementation Architecture

### PureScript Changes

1. **Add Handlebars FFI module** — New module `Handlebars.purs` with FFI to call `handlebars.compile()` and render templates

2. **Refactor data preparation** — Extract functions that prepare JSON-like data structures:
   - `preparePostData :: FormattedMarkdownData -> PostContext`
   - `prepareIndexData :: Array FrontMatterS -> IndexContext`
   - `prepareArchiveData :: Array FrontMatterS -> ArchiveContext`

3. **Remove HTML generation code** — Delete functions that currently build HTML strings:
   - `recentPosts`
   - `groupedPostsToHTML`
   - `convertCategoriesToString`
   - Related helper functions

4. **Update build flow:**
   - Read template files (`.hbs`)
   - Compile templates once at start of build
   - For each output, prepare data → render with compiled template → write file

### Bundling

- Add `handlebars` as npm dependency
- Bundler (esbuild/webpack) includes it in `vaak.cjs`
- Users don't need to install anything extra

## Build Sequence

1. Create tmp folder
2. Build `index.hbs` → `tmp/index.html`
3. Build `archive.hbs` → `tmp/archive.html`
4. Build `feed.xml` → `tmp/feed.xml`
5. Build `post.hbs` for each post → `tmp/{slug}.html`
6. Build `404.hbs` → `tmp/404.html`
7. Copy static assets (images, js, style.css) to tmp

**On failure at any step:**
- Clear tmp folder
- Exit with error message

**On success:**
- Copy tmp folder contents to `OUTPUT_DIR` (env variable, default `./public`)

## Error Handling

### Template loading errors
- Missing `.hbs` file → Fail with: `"Error: Template file not found: templates/post.hbs"`
- File read error → Fail with underlying error message

### Template compilation errors
- Handlebars syntax error → Fail with: `"Error: Invalid template syntax in post.hbs: [handlebars error message]"`

### Render errors
- Missing variable → Handlebars renders empty string (default behavior, not an error)

### Build validation
- Load and compile all 4 templates upfront before generating any output
- If any template fails to load/compile, stop immediately
- Ensures users don't get partial builds

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| RSS template | Keep as-is | XML is different from HTML, current approach works |
| Partials support | No | Keep it simple, each template self-contained |
| Handlebars integration | Bundle into vaak.cjs | Users don't need extra dependencies |
| Date formatting | Pre-formatted in data | Simpler templates, no custom helpers needed |
| Template extension | `.hbs` | Clear signal these are Handlebars templates |
| Migration path | Clean break | Simpler implementation, document in changelog |
| post.md template | Keep as-is | Markdown uses $placeholder syntax |
| Site config | siteUrl from SITE_URL env | Minimal config, already works |

## Breaking Changes

This is a breaking change. Existing vaak users must:

1. Run `vaak init` to generate new `.hbs` templates
2. Manually migrate customizations from old `.html` templates to new `.hbs` format
3. Old `{{placeholder}}` syntax still works but templates must use `.hbs` extension
