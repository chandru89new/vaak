import matter from "gray-matter";
import dayjs from "dayjs";
import MarkdownIt from "markdown-it";
import TurndownService from "turndown";
import yaml from "js-yaml";
import fs from "fs";

const formatDate = (format) => (dateString) =>
  !!dateString ? dayjs(dateString).format(format) : dayjs().format(format);

const md2FormattedDataService = new MarkdownIt({ html: true });
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

const turndownService = new TurndownService();

const htmlToMarkdown = (htmlContent) => turndownService.turndown(htmlContent);

const getEnv = (key) => process.env[key] || "";

const getCategoriesJson = () => {
  try {
    return yaml.load(fs.readFileSync("./contents/categories.yml"), "utf-8");
  } catch {
    return [];
  }
};

export {
  md2RawFormattedData,
  formatDate,
  htmlToMarkdown,
  getEnv,
  getCategoriesJson
};
