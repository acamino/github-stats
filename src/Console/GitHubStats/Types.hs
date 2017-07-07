module Console.GitHubStats.Types where

import qualified Data.Text as T

type Language = T.Text

data Repository = Repository
  { repoLanguage :: Maybe Language
  }

data Organization = Organization
  { orgPublicRepos :: Maybe Integer
  }
