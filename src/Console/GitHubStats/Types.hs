{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Console.GitHubStats.Types where

import           Data.Aeson
import qualified Data.Text       as T
import           Numeric.Natural

type Language = T.Text

data Repository = Repository
  { repoLanguage :: Maybe Language
  }
  deriving (Eq, Show)

instance FromJSON Repository where
  parseJSON = withObject "repo in org" $ \o -> do
    repoLanguage <- o .: "language"
    return Repository{..}

data Organization = Organization
  { orgPublicRepos :: Maybe Natural
  }
  deriving (Eq, Show)

instance FromJSON Organization where
  parseJSON = withObject "organization" $ \o -> do
    orgPublicRepos <- o .: "public_repos"
    return Organization{..}
