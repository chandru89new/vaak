import matter from "gray-matter";
import dayjs from "dayjs";
import MarkdownIt from "markdown-it";
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
      collections: (r.data.collections?.split(",") ?? [])?.map((s) => s.trim()),
      status: r.data.status || "published"
    },
    content: md2FormattedDataService.render(r.content),
    raw: string
  };
};

export { md2RawFormattedData, formatDate };
