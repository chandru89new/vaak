import Handlebars from "handlebars";

// compileTemplate :: String -> (Foreign -> String)
const compileTemplate = (templateString) => {
  const compiled = Handlebars.compile(templateString);
  return (context) => compiled(context);
};

// renderTemplate :: (Foreign -> String) -> Foreign -> String
const renderTemplate = (compiledFn) => (context) => compiledFn(context);

export { compileTemplate, renderTemplate };
