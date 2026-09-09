# Changelog

## v0.12.0 (2026-09-09)

- Upgrade to new spago build tool
- Add footnotes support in markdown rendering
- Update README with prerequisites section

## v0.11.0 (2026-02-11)

- Add `rebuild` command
- Update help text and templates
- Add CNAME for GitHub Pages publishing

## v0.10.2 (2026-02-08)

- Add documentation

## v0.10.1 (2026-02-05)

- Fix posts data not being passed correctly in all templates

## v0.10.0 (2026-02-05)

- Add support for collections

## v0.9.3 (2026-02-02)

- Upgrade spago package set and fix resulting errors
- Add `postsByYear` to index template context
- Catch template rendering issues in Nunjucks as proper errors
- Exit with warning if vaak already initialized in folder

## v0.9.2 (2026-01-14)

- Fix re-init behavior

## v0.9.1 (2025-12-25)

- Add example site
- Update README

## v0.9.0 (2025-12-18)

- **Breaking:** Replace hardcoded templates with Nunjucks templating
- Add `SITE_URL` and `SITE_NAME` as required environment variables
- Add `RECENT_POSTS` env var to limit home page posts
- Upgrade markdown-it to v14
- Move context preparation from JS to PureScript

## v0.8.6 (2025-11-02)

- Render headings with links
- Remove redundant Maybe type

## v0.8.5 (2025-08-30)

- Update error messages

## v0.8.4 (2025-07-30)

- Add `unlisted` post status for sharing drafts with others

## v0.8.3 (2025-04-19)

- Ensure posts converted from published to draft get deleted from output folder

## v0.8.2 (2025-04-18)

- Remove use of `getConfig` after switching to ReaderT

## v0.8.1 (2025-04-17)

- Refactor to use ReaderT for config management

## v0.8.0 (2025-04-15)

- Add `init` command for project scaffolding
- Make domain config a `Maybe` type
- Generalize `askConfig` to `MonadEffect`
- Add README

## v0.7.1 (2025-04-13)

- Fix release workflow
- Fix version number

## v0.7.0 (2025-04-12)

- Use Tailwind standalone CLI instead of npm-based approach

## v0.6.0 (2025-04-12)

- Refactor codebase

## v0.5.1 (2025-04-08)

- Fix bug in CSS generation through Tailwind

## v0.5.0 (2025-03-02)

- Add customizable folder paths
- Add help text

## v0.4.0 (2025-03-01)

- **Breaking:** Use `status` field instead of `ignore` for filtering posts

## v0.3.2 (2025-02-28)

- Use status flag for post filtering

## v0.3.1 (2025-02-16)

- Update logging copy

## v0.3.0 (2025-02-16)

- Improve logging output
- Set executable permissions on bundled output

## v0.2.0 (2025-02-09)

- Move `ExceptT` to top-level functions
- Fix error handling to throw correctly

## v0.1.2 (2025-02-09)

- Add node_modules cleanup step to build

## v0.1.1 (2025-02-07)

- General cleanup
- Update GitHub Action

## v0.1.0 (2025-02-07)

- Initial release
- Set up CI/CD release workflow
