{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Console.GitHubStats.Types where

import           Data.Aeson
import           Data.Text       (Text)
import           Numeric.Natural

type Language = Text

data Repository = Repository
  { repoLanguage :: Maybe Language
  } deriving (Eq, Show)

instance FromJSON Repository where
  parseJSON = withObject "Repository" $ \o ->
    Repository <$> o .: "language"

data Organization = Organization
  { orgPublicRepos :: Maybe Natural
  } deriving (Eq, Show)

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \o ->
    Organization <$> o .: "public_repos"
