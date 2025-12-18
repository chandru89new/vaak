module Nunjucks where

import Foreign (Foreign)

-- Render a template file with context data
-- Takes file path (e.g., "./templates/index.html") and context object, returns HTML string
foreign import render :: String -> Foreign -> String
