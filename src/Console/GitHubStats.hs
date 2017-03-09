{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Console.GitHubStats
  ( fetchRepos
  ) where

import Data.Aeson
import Data.Monoid ((<>))
import Network.HTTP.Req
import qualified Data.Text as T

import Console.GitHubStats.Types

instance FromJSON Repository where
  parseJSON = withObject "repo in org" $ \o -> do
    repoLanguage <- o .: "language"
    return Repository {..}

fetchRepos :: MonadHttp m
  => T.Text            -- ^ Github organization name
  -> m [Repository]
fetchRepos orgName = do
  let gitHubApi    = https "api.github.com"
      listReposUrl = gitHubApi /: "orgs" /: orgName /: "repos"
      params =
        "per_page" =: (100 :: Int) <>
        header "User-Agent" "ghs"
  repos <- req GET listReposUrl NoReqBody jsonResponse params
  return (responseBody repos :: [Repository])
