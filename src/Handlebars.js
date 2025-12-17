import Handlebars from "handlebars";
import fs from "fs";

// render :: String -> Foreign -> String
// Takes template filename and context, returns rendered HTML
const render = (filename) => (context) => {
  const template = fs.readFileSync("./templates/" + filename, "utf8");
  return Handlebars.compile(template)(context);
};

export { render };
