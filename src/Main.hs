{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import Control.Arrow ((&&&))
import Control.Exception (throwIO)
import Data.Aeson (FromJSON)
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
data Repository = Repository { language :: Maybe Language
                             } deriving (Generic)

instance FromJSON Repository

main = do
  repos <- fetchRepositories "stackbuilders"
  let langs        = mapMaybe language repos
      groupedLangs = group . sort $ langs
      stats        = getStats groupedLangs
  mapM_ T.putStrLn $ histogram stats

fetchRepositories :: MonadHttp m
  => T.Text            -- ^ Github organization name
  -> m [Repository]
fetchRepositories orgName = do
  let gitHubApi           = https "api.github.com"
      listRepositoriesUrl = gitHubApi /: "orgs" /: orgName /: "repos"
      params =
        "per_page" =: (100 :: Int) <>
        header "User-Agent" "ghs"
  repos <- req GET listRepositoriesUrl NoReqBody jsonResponse params
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
