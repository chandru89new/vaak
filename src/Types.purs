module Types where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Exception (Error)

-- Types from Utils.purs
type RawFormattedMarkdownData =
  { frontMatter :: RawFrontMatter
  , content :: String
  , raw :: String
  }

type FrontMatter a =
  { title :: String
  , date :: String
  , slug :: String
  , tags :: Array String
  , status :: a
  }

type FormattedMarkdownData =
  { frontMatter :: FrontMatterS
  , content :: String
  , raw :: String
  }

type RawFrontMatter = FrontMatter String
type FrontMatterS = FrontMatter Status

type Category =
  { category :: String
  , posts :: Array String
  }

data Status
  = Draft
  | Published
  | Unlisted
  | InvalidStatus String

-- Derive instances for Status
derive instance eqStatus :: Eq Status
derive instance ordStatus :: Ord Status

instance showStatus :: Show Status where
  show Draft = "Draft"
  show Published = "Published"
  show Unlisted = "Unlisted"
  show (InvalidStatus s) = "InvalidStatus" <> show s

type Config =
  { templateFolder :: String
  , outputFolder :: String
  , contentFolder :: String
  , blogPostTemplate :: String
  , totalRecentPosts :: Int
  , domain :: Maybe String
  }

-- Types from Main.purs
newtype Template = Template String

data Command
  = Build
  | ShowVersion
  | Help
  | NewPost String
  | Init
  | Invalid
  | Test

instance showCommand :: Show Command where
  show Build = "Build"
  show Test = "Test"
  show Init = "Init"
  show Help = "Help"
  show (NewPost _) = "NewPost"
  show Invalid = "Invalid"
  show ShowVersion = "ShowVersion"

type AppM a = ReaderT Config (ExceptT Error Aff) a
