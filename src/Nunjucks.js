import nunjucks from "nunjucks";
import fs from "fs";

// render :: String -> Foreign -> String
// Takes template file path and context, returns rendered HTML
const render = (filePath) => (context) => () => {
  return new Promise((res, rej) => {
    try {
      const template = fs.readFileSync(filePath, "utf8");
      res(nunjucks.renderString(template, context));
    } catch (e) {
      rej(e);
      return;
    }
  });
};

export { render };
