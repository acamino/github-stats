{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -fno-warn-orphans  #-}
{-# LANGUAGE DataKinds         #-}

module Console.GitHubStats
  ( fetchRepos
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Network.HTTP.Req

import           Console.GitHubStats.Types

instance FromJSON Repository where
  parseJSON = withObject "repo in org" $ \o -> do
    repoLanguage <- o .: "language"
    return Repository{..}

instance FromJSON Organization where
  parseJSON = withObject "organization" $ \o -> do
    orgPublicRepos <- o .: "public_repos"
    return Organization{..}

perPage :: Integer
perPage = 100

fetchRepos :: MonadHttp m
  => T.Text            -- ^ Github organization name
  -> m [Repository]
fetchRepos orgName = do
  numberOfRepos <- fetchNumberOfRepos orgName
  let n = numberOfRepos `div` perPage
      pages = case numberOfRepos `mod` perPage of
              0 -> n
              _ -> n + 1
  repos <- forM [1..pages] $ fetchReposByPage orgName
  return $ concat repos

fetchReposByPage :: MonadHttp m
  => T.Text
  -> Integer
  -> m [Repository]
fetchReposByPage orgName page = do
  let listReposUrl = gitHubApi /: "orgs" /: orgName /: "repos"
      params = "per_page" =: perPage <>
               "page" =: page <>
               header "User-Agent" "github-stats"
  repos <- req GET listReposUrl NoReqBody jsonResponse params
  return (responseBody repos :: [Repository])

fetchNumberOfRepos :: MonadHttp m
  => T.Text
  -> m Integer
fetchNumberOfRepos orgName = do
  let listReposUrl = gitHubApi /: "orgs" /: orgName
      params       = header "User-Agent" "github-stats"
  repos <- req GET listReposUrl NoReqBody jsonResponse params
  let organization = responseBody repos :: Organization
  return $ fromMaybe 0 $ orgPublicRepos organization

gitHubApi :: Url 'Https
gitHubApi = https "api.github.com"