module Handlebars where

import Foreign (Foreign)

-- A compiled Handlebars template function
foreign import data CompiledTemplate :: Type

-- Compile a template string into a reusable template function
foreign import compileTemplate :: String -> CompiledTemplate

-- Render a compiled template with a context object
foreign import renderTemplate :: CompiledTemplate -> Foreign -> String
