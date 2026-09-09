import matter from "gray-matter";
import dayjs from "dayjs";
import MarkdownIt from "markdown-it";
import markdownItAnchor from "markdown-it-anchor";
import markdownItFootnote from "markdown-it-footnote";

const formatDate = (format) => (dateString) =>
  !!dateString ? dayjs(dateString).format(format) : dayjs().format(format);

const md2FormattedDataService = new MarkdownIt({ html: true })
  .use(markdownItAnchor, {
    permalink: markdownItAnchor.permalink["headerLink"](),
  })
  .use(markdownItFootnote);

md2FormattedDataService.renderer.rules.footnote_caption = (
  tokens,
  idx,
  options,
  env,
  self,
) => {
  let n = Number(tokens[idx].meta.id + 1).toString();
  if (tokens[idx].meta.subId > 0) n += `:${tokens[idx].meta.subId}`;
  return `${n}`;
};

const md2RawFormattedData = (string) => {
  const r = matter(string);
  return {
    frontMatter: {
      ...r.data,
      date: formatDate("YYYY-MM-DD")(r.data.date),
      collections: (r.data.collections?.split(",") ?? [])?.map((s) => s.trim()),
      status: r.data.status || "published",
    },
    content: md2FormattedDataService.render(r.content),
    raw: string,
  };
};

export { md2RawFormattedData, formatDate };
