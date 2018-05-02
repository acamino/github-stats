{-# LANGUAGE OverloadedStrings #-}

module Console.GitHubStats.Stats where

import           Control.Arrow
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Console.GitHubStats.Types

mkHistogram :: [Repository] -> [Text]
mkHistogram = map renderBar . mkStats . groupLangs
  where
    renderBar (language, quantity) =
      T.unwords [ T.replicate quantity "#"
                , language
                , T.pack (show quantity)
                ]

    mkStats    = sortBy (comparing $ Down . snd) . map (head &&& length)
    groupLangs = group . sort . mapMaybe repoLanguage
