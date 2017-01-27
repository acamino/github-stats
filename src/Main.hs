{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON)
import Data.List (group, sort, sortBy)
import Data.Maybe (catMaybes)
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
                             } deriving (Show, Generic)

instance FromJSON Repository

main = do
  r <- req GET
    (https "api.github.com" /: "orgs" /: "stackbuilders" /: "repos")
    NoReqBody
    jsonResponse
    (header "User-Agent" "ac" <> "per_page" =: T.pack "100")
  let repos        = responseBody r :: Maybe [Repository]
      langs        = getLanguages repos
      groupedLangs = group . sort $ langs
      stats        = getStats groupedLangs
  mapM_ T.putStrLn $ histogram stats

getLanguages :: Maybe [Repository] -> [Language]
getLanguages repos =
  case repos of
    Nothing    -> []
    Just repos -> catMaybes . map language $ repos

getStats :: [[Language]] -> [(Language, Int)]
getStats groupedLangs =
  let stats = map (\ls -> (head ls, length ls)) groupedLangs
  in sortBy (comparing (Down . snd)) stats


histogram :: [(Language, Int)] -> [T.Text]
histogram =
  let bar (language, quantity) = T.pack (replicate quantity '#')
                              <> " "
                              <> language
                              <> " "
                              <> T.pack (show quantity)
  in map bar
