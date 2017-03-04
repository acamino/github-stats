{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Arrow ((&&&))
import Control.Exception (throwIO)
import Data.Aeson
import Data.List (group, sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (Down(..), comparing)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.Text as T
import qualified Data.Text.IO as T

instance MonadHttp IO where
  handleHttpException = throwIO

type Language = T.Text
data Repository = Repository
  { repoLanguage :: Maybe Language
  }

instance FromJSON Repository where
  parseJSON = withObject "repo in org" $ \o -> do
    repoLanguage <- o .: "language"
    return Repository {..}

main = do
  repos <- fetchRepos "stackbuilders"
  let langs        = mapMaybe repoLanguage repos
      groupedLangs = group . sort $ langs
      stats        = getStats groupedLangs
  mapM_ T.putStrLn $ histogram stats

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

getStats :: [[Language]] -> [(Language, Int)]
getStats groupedLangs =
  let stats = map (head &&& length) groupedLangs
  in sortBy (comparing $ Down . snd) stats

histogram :: [(Language, Int)] -> [T.Text]
histogram =
  let bar (language, quantity) = T.pack (replicate quantity '#') <>
                                 " "                             <>
                                 language                        <>
                                 " "                             <>
                                 T.pack (show quantity)
  in map bar
