{-# LANGUAGE OverloadedStrings #-}

module Console.GitHubStats.Stats where

import           Control.Arrow
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text                 as T

import           Console.GitHubStats.Types

histogram :: [Repository] -> [T.Text]
histogram repos = map renderBar stats
  where
    renderBar (language, quantity) =
      T.replicate quantity "#" <>
      " "                      <>
      language                 <>
      " "                      <>
      T.pack (show quantity)

    stats  = toStats . groupLangs $ repos

    groupLangs :: [Repository] -> [[Language]]
    groupLangs = group . sort . mapMaybe repoLanguage

    toStats :: [[Language]] -> [(Language, Int)]
    toStats = sortBy (comparing $ Down . snd) . map (head &&& length)
