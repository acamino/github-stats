{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad
import Data.Aeson
import Data.List
import Data.Ord
import GHC.Generics
import Network.HTTP.Req

instance MonadHttp IO where
  handleHttpException = throwIO

type Language = String
data Repository =
  Repository { language :: Maybe Language }
             deriving (Show, Generic)

instance FromJSON Repository

main = do
  r <- req GET
    (https "api.github.com" /: "orgs" /: "stackbuilders" /: "repos")
    NoReqBody
    jsonResponse
    (header "User-Agent" "ac")
  let repos = responseBody r :: Maybe [Repository]
      langs = (getLangs . getRepos) repos
      groupedLangs = (group . sort . filter (/= "")) langs
      xs = sortBy (comparing (Down . snd)) $ map (\xs -> (head xs, length xs)) groupedLangs
  print xs
  mapM_ putStrLn (histogram xs) 

getRepos :: Maybe [Repository] -> [Repository]
getRepos (Just r) = r
getRepos Nothing  = []

getLangs :: [Repository] -> [Language]
getLangs repos = map getLang repos

getLang :: Repository -> Language
getLang (Repository {language = (Just lang)}) = lang  
getLang (Repository {language = Nothing})     = ""

histogram :: [(Language, Int)] -> [String]
histogram = map (\x -> bar (snd x) ++ " " ++ fst x ++ " " ++ show (snd x))
  where bar n = take n $ repeat '#'
