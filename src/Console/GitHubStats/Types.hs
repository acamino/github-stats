module Console.GitHubStats.Types where

import qualified Data.Text as T

type Language = T.Text

-- | GitHub repository
data Repository = Repository
  { repoLanguage :: Maybe Language
  }
