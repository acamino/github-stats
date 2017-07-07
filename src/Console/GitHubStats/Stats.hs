{-# LANGUAGE OverloadedStrings #-}

module Console.GitHubStats.Stats where

import           Control.Arrow
import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Text                 as T

import           Console.GitHubStats.Types

histogram :: [Repository] -> [T.Text]
histogram repos = fmap renderBar stats
  where
    renderBar (language, quantity) =
      T.unwords [ T.replicate quantity "#"
                , language
                , T.pack (show quantity)
                ]

    stats = toStats . groupLangs $ repos

    toStats    = sortBy (comparing $ Down . snd) . fmap (head &&& length)
    groupLangs = group . sort . mapMaybe repoLanguage
