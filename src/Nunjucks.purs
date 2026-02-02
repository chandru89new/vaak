module Nunjucks where

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)

-- Render a template file with context data
-- Takes file path (e.g., "./templates/index.html") and context object, returns HTML string
foreign import render :: String -> Foreign -> Effect (Promise String)

renderTemplate :: String -> Foreign -> Aff String
renderTemplate templatePath context = toAffE (render templatePath context)