{-# LANGUAGE OverloadedStrings #-}

module Console.GitHubStats.Stats
  ( histogram
  ) where

import Control.Arrow ((&&&))
import Data.List (group, sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (Down(..), comparing)
import qualified Data.Text as T

import Console.GitHubStats.Types

histogram :: [Repository] -> [T.Text]
histogram repos =
  let groupedLangs             = getGroupedLangs repos
      stats                    = getStats groupedLangs
      bar (language, quantity) = T.replicate quantity "#"
                              <> " "
                              <> language
                              <> " "
                              <> T.pack (show quantity)
  in map bar stats

getGroupedLangs :: [Repository] -> [[Language]]
getGroupedLangs repos =
  let langs = mapMaybe repoLanguage repos
  in  group . sort $ langs

getStats :: [[Language]] -> [(Language, Int)]
getStats groupedLangs =
  let stats = map (head &&& length) groupedLangs
  in sortBy (comparing $ Down . snd) stats
