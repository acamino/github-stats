{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad
import Data.Aeson
import Data.List
import Data.Maybe
import Data.Ord
import GHC.Generics
import Network.HTTP.Req
import qualified Data.Text as T

instance MonadHttp IO where
  handleHttpException = throwIO

type Language = T.Text
data Repository =
  Repository { language :: Maybe Language
             } deriving (Show, Generic)

instance FromJSON Repository

main = do
  r <- req GET
    (https "api.github.com" /: "orgs" /: "stackbuilders" /: "repos")
    NoReqBody
    jsonResponse
    (header "User-Agent" "ac")
  let repos        = responseBody r :: Maybe [Repository]
      langs        = getLanguages repos
      groupedLangs = group . sort $ langs
      stats        = getStats groupedLangs
  mapM_ putStrLn $ histogram stats

getLanguages :: Maybe [Repository] -> [Language]
getLanguages repos =
  case repos of
    Nothing    -> []
    Just repos ->
      catMaybes . map (\repo -> getLanguage repo) $ repos
      where getLanguage (Repository language) = language

getStats :: [[Language]] -> [(Language, Int)]
getStats groupedLangs =
  let stats = map (\ls -> (head ls, length ls)) groupedLangs
  in sortBy (comparing (Down . snd)) stats

histogram :: [(Language, Int)] -> [String]
histogram =
  let bar (language, quantity) = bar' quantity ++ " " ++ T.unpack language ++ " " ++ show quantity
      bar' n = take n $ repeat '#'
  in map (\x -> bar x)
