import Handlebars from "handlebars";
import fs from "fs";

// render :: String -> Foreign -> String
// Takes template file path and context, returns rendered HTML
const render = (filePath) => (context) => {
  const template = fs.readFileSync(filePath, "utf8");
  return Handlebars.compile(template)(context);
};

export { render };
