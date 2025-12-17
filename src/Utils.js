import matter from "gray-matter";
import dayjs from "dayjs";
import MarkdownIt from "markdown-it";
import yaml from "js-yaml";
import fs from "fs";
import markdownItAnchor from "markdown-it-anchor";

const formatDate = (format) => (dateString) =>
  !!dateString ? dayjs(dateString).format(format) : dayjs().format(format);

const md2FormattedDataService = new MarkdownIt({ html: true }).use(
  markdownItAnchor,
  {
    permalink: markdownItAnchor.permalink["headerLink"]()
  }
);

const md2RawFormattedData = (string) => {
  const r = matter(string);
  return {
    frontMatter: {
      ...r.data,
      date: formatDate("YYYY-MM-DD")(r.data.date),
      tags: r.data.tags?.split(",") ?? [],
      status: r.data.status || "published"
    },
    content: md2FormattedDataService.render(r.content),
    raw: string
  };
};

// Prepare context for post.hbs template
const preparePostContext = (formatFn) => (frontMatter) => (content) => (siteUrl) => ({
  title: frontMatter.title,
  date: formatFn("MMM DD, YYYY")(frontMatter.date),
  slug: frontMatter.slug,
  content: content,
  siteUrl: siteUrl || ""
});

// Prepare context for index.hbs template
const prepareIndexContext = (formatFn) => (allPosts) => (siteUrl) => ({
  allPosts: allPosts.map(post => ({
    title: post.title,
    date: formatFn("MMM DD, YYYY")(post.date),
    slug: post.slug
  })),
  siteUrl: siteUrl || ""
});

// Prepare context for archive.hbs template
const prepareArchiveContext = (formatFn) => (groupedPosts) => (siteUrl) => ({
  postsByYear: groupedPosts.map(group => ({
    year: group.year,
    posts: group.posts.map(post => ({
      title: post.title,
      date: formatFn("MMM DD, YYYY")(post.date),
      slug: post.slug
    }))
  })),
  siteUrl: siteUrl || ""
});

// Prepare context for 404.hbs template
const prepare404Context = (siteUrl) => ({
  siteUrl: siteUrl || ""
});

export {
  md2RawFormattedData,
  formatDate,
  preparePostContext,
  prepareIndexContext,
  prepareArchiveContext,
  prepare404Context
};
