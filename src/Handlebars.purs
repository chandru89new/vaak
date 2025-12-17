module Handlebars where

import Foreign (Foreign)

-- Render a template file with context data
-- Takes filename (e.g., "index.hbs") and context object, returns HTML string
foreign import render :: String -> Foreign -> String
