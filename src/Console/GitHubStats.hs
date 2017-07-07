{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Console.GitHubStats
  ( fetchRepos
  ) where

import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Network.HTTP.Req

import           Console.GitHubStats.Types

fetchRepos :: MonadHttp m
  => T.Text            -- ^ Github organization name
  -> m [Repository]
fetchRepos orgName = do
  numberOfRepos <- fetchNumberOfRepos orgName
  let n     = numberOfRepos `div` perPage
      pages = if numberOfRepos `mod` perPage == 0
                 then n
                 else n + 1
  repos <- forM [1..pages] $ fetchReposByPage orgName
  return $ concat repos


fetchReposByPage :: MonadHttp m
  => T.Text
  -> Integer
  -> m [Repository]
fetchReposByPage orgName page = do
  let listReposUrl = gitHubApiUrl /: "orgs" /: orgName /: "repos"
      params       = "per_page" =: perPage <>
                     "page" =: page <>
                     header "User-Agent" "github-stats"
  repos <- req GET listReposUrl NoReqBody jsonResponse params
  return $ responseBody repos


fetchNumberOfRepos :: MonadHttp m
  => T.Text
  -> m Integer
fetchNumberOfRepos orgName = do
  let listReposUrl = gitHubApiUrl /: "orgs" /: orgName
      params       = header "User-Agent" "github-stats"
  repos <- req GET listReposUrl NoReqBody jsonResponse params
  return $ fromMaybe 0 . orgPublicRepos $ responseBody repos


gitHubApiUrl :: Url 'Https
gitHubApiUrl = https "api.github.com"


perPage :: Integer
perPage = 100
