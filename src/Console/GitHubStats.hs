{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Console.GitHubStats
  ( fetchRepos
  ) where

import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import           Network.HTTP.Req
import           Numeric.Natural

import           Console.GitHubStats.Types

fetchRepos :: MonadHttp m
  => Text              -- ^ Github organization name
  -> m [Repository]
fetchRepos orgName = do
  numberOfRepos <- fetchNumberOfRepos orgName
  let pages = countPages numberOfRepos
  repos <- forM [1..pages] $ fetchReposByPage orgName
  return $ concat repos
  where
    countPages numberOfRepos =
      ceiling $ toFloat numberOfRepos / toFloat perPage
    toFloat = fromIntegral :: (Natural -> Float)


fetchReposByPage :: MonadHttp m
  => Text
  -> Natural
  -> m [Repository]
fetchReposByPage orgName page = do
  let listReposUrl = gitHubApiUrl /: "orgs" /: orgName /: "repos"
      params       = "per_page" =: perPage <>
                     "page" =: page <>
                     header "User-Agent" "github-stats"
  repos <- req GET listReposUrl NoReqBody jsonResponse params
  return $ responseBody repos


fetchNumberOfRepos :: MonadHttp m
  => Text
  -> m Natural
fetchNumberOfRepos orgName = do
  let listReposUrl = gitHubApiUrl /: "orgs" /: orgName
      params       = header "User-Agent" "github-stats"
  repos <- req GET listReposUrl NoReqBody jsonResponse params
  return $ fromMaybe 0 . orgPublicRepos $ responseBody repos


gitHubApiUrl :: Url 'Https
gitHubApiUrl = https "api.github.com"

perPage :: Natural
perPage = 100
